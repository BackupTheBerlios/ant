
open XNum;

class type consumer =
object
  method consume : !'a . ('a -> (string * 'a)) -> 'a -> 'a;
end;

class type producer =
object
  method produce : !'a . ('a -> string -> 'a) -> 'a -> 'a;
end;

class type random_access =
object
  method pos  : int;
  method size : int;
  method seek : int -> unit;
end;

class type basic_stream =
object
  method free : unit;
end;

class type virtual istream =
object
  inherit basic_stream;
  inherit producer;
  method virtual eof : bool;
  method read_char   : char;
  method read_string : int -> string;
end;

class type virtual ostream =
object
  inherit basic_stream;
  inherit consumer;
  method virtual bytes_written : int;
  method write_char   : char -> unit;
  method write_string : string -> unit;
end;

class type virtual irstream =
object
  inherit istream;
  inherit random_access;
end;

class type virtual orstream =
object
  inherit ostream;
  inherit random_access;
end;

class type virtual istream_consumer =
object
  inherit istream;
  inherit consumer;
end;

class type virtual ostream_producer =
object
  inherit ostream;
  inherit producer;
end;

class type virtual iostream =
object
  inherit istream;
  inherit ostream;
end;

class type virtual iorstream =
object
  inherit iostream;
  inherit random_access;
end;

(* IO routines *)

value size          : #random_access -> int;
value pos           : #random_access -> int;
value seek          : #random_access -> int -> unit;
value skip          : #istream -> int -> unit;
value bytes_written : #ostream -> int;
value eof           : #istream -> bool;
value free          : #basic_stream -> unit;

(* Append the contents of a channel to a stream. *)

value append_channel : #consumer -> in_channel -> unit;

value append : #ostream -> #producer -> unit;

(* Write the contents of a stream to a channel. *)

value to_channel : #producer -> out_channel -> unit;

value to_string   : #producer -> string;
value from_string : string -> iorstream;
value to_buffer   : #producer -> iorstream;

value sub_stream : #istream -> int -> iorstream;

(* reading from a stream *)

value read_char   : #istream -> char;
value read_byte   : #istream -> int;
value read_string : #istream -> int -> string;

value peek_char   : #irstream -> int -> char;
value peek_string : #irstream -> int -> int -> string;
value skip_while  : #irstream -> (char -> bool) -> unit;

(* reading bigendian integers *)

value read_be_u8  : #istream -> int;
value read_be_u16 : #istream -> int;
value read_be_u24 : #istream -> int;
value read_be_u32 : #istream -> num;
value read_be_i8  : #istream -> int;
value read_be_i16 : #istream -> int;
value read_be_i24 : #istream -> int;
value read_be_i32 : #istream -> num;

value read_utf8_char : #istream -> int;

(* writing to a stream *)

value write_char   : #ostream -> char -> unit;
value write_byte   : #ostream -> int -> unit;
value write_string : #ostream -> string -> unit;

value printf       : #ostream -> format4 'a unit string unit -> 'a;

value write_be_u8  : #ostream -> int -> unit;
value write_be_u16 : #ostream -> int -> unit;
value write_be_u24 : #ostream -> int -> unit;
value write_be_u32 : #ostream -> num -> unit;
value write_be_i8  : #ostream -> int -> unit;
value write_be_i16 : #ostream -> int -> unit;
value write_be_i24 : #ostream -> int -> unit;
value write_be_i32 : #ostream -> num -> unit;

value write_utf8_char : #ostream -> int -> unit;

(* implementations *)

value make_in_stream      : string -> istream;
value make_out_stream     : string -> ostream;
value make_rand_in_stream : string -> irstream;
value make_buffer_stream  : int -> iorstream;
value make_string_stream  : string -> irstream;

value compress   : #iorstream -> int -> iorstream;
value uncompress : #iorstream -> iorstream;

