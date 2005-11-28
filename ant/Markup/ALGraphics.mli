
open Runtime;
open VM;
open Types;
open Typesetting;
open Engine;
open Environment;
open Evaluate;

value encode_gfx_cmd : Graphic.graphic_command dim_arg Box.box -> partial_value;
value decode_gfx_cmd : string -> unknown -> Graphic.graphic_command dim_arg Box.box;

