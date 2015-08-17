#use "list.ml"

let table = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/"

let base64 cs =
  let rec aux n1 b1 cs = match cs with
    | [] -> if n1 = 0 then [] else [b1 lsl (6 - n1)]
    | c1 :: cs ->
      let c2 = ((b1 lsl 8) lor c1) lsr (n1 + 2) in
      let b2 = c1 land (0xff lsr (6 - n1)) in
      if n1 = 4 then c2 :: b2 :: aux 0 0 cs else c2 :: aux (n1+2) b2 cs
  in
  list_map (list_nth table) (aux 0 0 (list_map int_of_char cs))

let str = base64 "Compile-time BASE64 encoding!"
let len = list_length str

(*!
#include <cassert>
#include <cstring>

int main (void) {
  char buf[len::val + 1];
  __ml_array_of_list<char, str>::set(buf);
  assert(std::strcmp(buf, "Q29tcGlsZS10aW1lIEJBU0U2NCBlbmNvZGluZyE") == 0);
  return 0;
}
*)
