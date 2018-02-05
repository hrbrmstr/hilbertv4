#include <Rcpp.h>

using namespace Rcpp;

void ip_to_xy(unsigned ip, unsigned *x, unsigned *y,
              int addr_space_bits_per_image = 32L,
              int addr_space_bits_per_pixel = 8L) {

  int i;
  int order = (addr_space_bits_per_image - addr_space_bits_per_pixel) / 2L;
  unsigned int state, row, s;

  unsigned int addr_space_first_addr = 0L;

  s = (((unsigned)ip) - addr_space_first_addr) >> addr_space_bits_per_pixel;

  state = *x = *y = 0L;

  for (i = 2L * order - 2L; i >= 0; i -= 2L) {

    row = 4 * state | ((s >> i) & 3);

    *x = (*x << 1L) | ((0x936C >> row) & 1L);
    *y = (*y << 1L) | ((0x39C6 >> row) & 1L);

    state = (0x3E6B94C1 >> 2L * row) & 3L;

  }

}

// [[Rcpp::export]]
List ips_to_xy(std::vector<unsigned> ips,
               int addr_space_bits_per_image = 32L,
               int addr_space_bits_per_pixel = 8L) {

  unsigned x, y;

  std::vector<unsigned> out_x(ips.size());
  std::vector<unsigned> out_y(ips.size());

  for (unsigned int j=0; j<ips.size(); j++) {

    ip_to_xy((unsigned)ips[j], &x, &y, addr_space_bits_per_image, addr_space_bits_per_pixel);

    out_x[j] = x;
    out_y[j] = y;

  }

  DataFrame out = DataFrame::create(Named("x", out_x), Named("y", out_y));

  out.attr("class") = CharacterVector::create("tbl_df", "data.frame") ;

  return(out);

}