
open XNum;

type pdf_value =
[ Null
| Bool of bool
| Int of int
| Float of float
| String of IO.iorstream
| Stream of pdf_dictionary and IO.iorstream
| Symbol of string
| Array of list pdf_value
| Dictionary of pdf_dictionary
| Reference of int and int
]
and pdf_dictionary = list (string * pdf_value);

type pdf_file 'a = 'b;

value create_pdf       : 'a -> float -> pdf_file 'a;
value alloc_object     : pdf_file 'a -> int;
value set_root         : pdf_file 'a -> pdf_value -> unit;
value set_object       : pdf_file IO.ostream -> int -> pdf_value -> unit;

value read_pdf_file    : string -> pdf_file IO.irstream;
value create_pdf_file  : string -> float -> pdf_file IO.ostream;
value finish_pdf_file  : pdf_file IO.ostream -> unit;
value dict_lookup      : list (string * pdf_value) -> string -> pdf_value;
value lookup_reference : pdf_file IO.irstream -> pdf_value -> pdf_value;

