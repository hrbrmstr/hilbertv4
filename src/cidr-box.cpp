// [[Rcpp::depends(BH)]]
// [[Rcpp::depends(AsioHeaders)]]
#include <Rcpp.h>

#ifdef __APPLE__
#pragma clang diagnostic push
#pragma clang diagnostic ignored "-Wunused-local-typedef"
#endif
#include <asio.hpp>
#ifdef __APPLE__
#pragma clang diagnostic pop
#endif

using namespace Rcpp;

#ifndef MIN
#define MIN(a,b) (a<b?a:b)
#define MAX(a,b) (a>b?a:b)
#endif

unsigned int addr_space_first_addr = 0;
unsigned int addr_space_last_addr = ~0;
unsigned int allones = ~0;

struct bb {
  int xmin, ymin, xmax, ymax;
};

typedef struct bb bbox;

int cidr_parse(const char *cidr, unsigned int *rfirst, unsigned int *rlast, int *rslash);
void hil_xy_from_s(unsigned s, int order, unsigned *xp, unsigned *yp);
unsigned int xy_from_ip(unsigned ip, unsigned *xp, unsigned *yp, int addr_space_bits_per_image, int addr_space_bits_per_pixel);
bbox bbox_from_cidr(const char *cidr, int addr_space_bits_per_image, int addr_space_bits_per_pixel);
void (*xy_from_s) (unsigned s, int n, unsigned *xp, unsigned *yp) = hil_xy_from_s;

unsigned int xy_from_ip(unsigned ip, unsigned *xp, unsigned *yp,
                        int addr_space_bits_per_image = 32L,
                        int addr_space_bits_per_pixel = 8L) {

  unsigned int s;

  if (ip < addr_space_first_addr) return 0;
  if (ip > addr_space_last_addr) return 0;

  s = (ip - addr_space_first_addr) >> addr_space_bits_per_pixel;

  int hilbert_curve_order = (addr_space_bits_per_image - addr_space_bits_per_pixel) / 2;

  xy_from_s(s, hilbert_curve_order, xp, yp);

  return 1;

}


int cidr_parse(const char *cidr, unsigned int *rfirst, unsigned int *rlast, int *rslash) {

  char cidr_copy[24];
  char *t;
  int slash;
  unsigned int first;
  unsigned int last;

  strncpy(cidr_copy, cidr, 24);
  t = strchr(cidr_copy, '/');

  if (NULL == t) {
    Rcpp::warning(tfm::format("Missing / on CIDR '%s'", cidr_copy));
    return 0;;
  }

  *t++ = '\0';
  slash = atoi(t);

  try{
    first = asio::ip::address_v4::from_string(std::string(cidr_copy)).to_ulong();
  } catch(...) {
    Rcpp::warning("Failed to convert CIDR");
    return(0);
  }

  first = asio::detail::socket_ops::network_to_host_long(first);

  if (slash < 32)
    last = first | (allones >> slash);
  else
    last = first;

  *rfirst = first;
  *rlast = last;
  *rslash = slash;

  return 1;

}

void hil_xy_from_s(unsigned s, int order, unsigned *xp, unsigned *yp) {

  int i;
  unsigned state, x, y, row;

  state = 0; // Initialize
  x = y = 0;

  for (i = 2 * order - 2; i >= 0; i -= 2) {	// Do n times
    row = 4 * state | ((s >> i) & 3);	// Row in table
    x = (x << 1) | ((0x936C >> row) & 1);
    y = (y << 1) | ((0x39C6 >> row) & 1);
    state = (0x3E6B94C1 >> 2 * row) & 3;	// New state
  }

  // results
  *xp = x;
  *yp = y;
}

static bbox bounding_box(unsigned int first, int slash,
                         int addr_space_bits_per_image = 32L,
                         int addr_space_bits_per_pixel = 8L) {

  bbox box;
  unsigned int diag = 0xAAAAAAAA;
  unsigned int x1 = 0, y1 = 0, x2 = 0, y2 = 0;

  if (slash > 31) { // treat /32 as a special case

    xy_from_ip(first, &x1, &y1,
               addr_space_bits_per_image = 32L, addr_space_bits_per_pixel = 8L);

    box.xmin = x1;
    box.ymin = y1;
    box.xmax = x1;
    box.ymax = y1;

  } else if (0 == (slash & 1)) { // square

    diag >>= slash;

    xy_from_ip(first, &x1, &y1,
               addr_space_bits_per_image, addr_space_bits_per_pixel);

    xy_from_ip(first + diag, &x2, &y2,
               addr_space_bits_per_image, addr_space_bits_per_pixel);

    box.xmin = MIN(x1, x2);
    box.ymin = MIN(y1, y2);
    box.xmax = MAX(x1, x2);
    box.ymax = MAX(y1, y2);

  } else { // rectangle: divide, conquer

    bbox b1 = bounding_box(first, slash + 1,
                           addr_space_bits_per_image,
                           addr_space_bits_per_pixel);

    bbox b2 = bounding_box(first + (1 << (32 - (slash + 1))), slash + 1,
                           addr_space_bits_per_image,
                           addr_space_bits_per_pixel);

    box.xmin = MIN(b1.xmin, b2.xmin);
    box.ymin = MIN(b1.ymin, b2.ymin);
    box.xmax = MAX(b1.xmax, b2.xmax);
    box.ymax = MAX(b1.ymax, b2.ymax);
  }

  return box;

}

bbox bbox_from_cidr(const char *cidr,
                    int addr_space_bits_per_image = 32L,
                    int addr_space_bits_per_pixel = 8L) {

  int slash;
  unsigned int first;
  unsigned int last;
  bbox bbox;

  cidr_parse(cidr, &first, &last, &slash);

  if (first < addr_space_first_addr || last > addr_space_last_addr) {
    bbox.xmin = bbox.ymin = bbox.xmax = bbox.ymax = -1;
    return bbox;
  }

  memset(&bbox, '\0', sizeof(bbox));

  bbox = bounding_box(first, slash,
                      addr_space_bits_per_image,
                      addr_space_bits_per_pixel);

  return bbox;

}

//' Bounding box from CIDR blocks
//'
//' Returns a data frame of bounding boxes for a given CIDR within the
//' Hilbert-curve plane.
//'
//' @md
//' @param cidr character vector of dotted-decimal/digit CIDRs
//' @return list of bounding box extents for each CIDR - `cidr`,
//'         `xmin`, `ymin`, `xmax`, `ymax`
//' @export
//' @examples
//' bbox_cidr("30.0.0.0/8")
//[[Rcpp::export]]
List bbox_cidr(CharacterVector cidr,
               int addr_space_bits_per_image = 32L,
               int addr_space_bits_per_pixel = 8L) {

  bbox bbox;

  if (addr_space_last_addr == 0) addr_space_last_addr = allones;

  int cidr_count = cidr.size() ;

  IntegerVector xmin(cidr_count);
  IntegerVector ymin(cidr_count);
  IntegerVector xmax(cidr_count);
  IntegerVector ymax(cidr_count);

  for (int i=0; i<cidr_count; i++) {

    bbox = bbox_from_cidr(cidr[i], addr_space_bits_per_image, addr_space_bits_per_pixel);

    xmin[i] = bbox.xmin;
    ymin[i] = bbox.ymin;
    xmax[i] = bbox.xmax;
    ymax[i] = bbox.ymax;

  }

  DataFrame boxes = DataFrame::create(
    _["cidr"] = cidr,
    _["xmin"] = xmin,
    _["ymin"] = ymin,
    _["xmax"] = xmax,
    _["ymax"] = ymax
  );

  return(boxes);

}
