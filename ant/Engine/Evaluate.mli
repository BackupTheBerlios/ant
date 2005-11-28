
open XNum;
open Runtime;
open Unicode.Types;
open Dim;
open Typesetting;
open Box;
open Environment;

value tracing_engine : ref bool;


type area_contents_arg =
[= `Galley of (uc_string * skip_arg * skip_arg * skip_arg * skip_arg)
|  `Float of (FloatVertical.vert_alignment * skip_arg * skip_arg * dim_arg)
|  `Footnote of (list node_type * skip_arg * skip_arg * skip_arg *
                 line_param_modifier * par_param_modifier * line_break_param_modifier *
                 hyphen_param_modifier * space_param_modifier * math_param_modifier)
|  `Direct of page_info -> (num * num) -> list node_type
]
and node_type =
[= `Nodes of list node_type
|  `Command of (UCStream.location * env_cmd)
|  `CommandBox of (UCStream.location * box_cmd)
|  `GfxCommand of (UCStream.location * Graphic.graphic_command dim_arg box)
|  `NewGalley of (UCStream.location * uc_string * skip_arg)
|  `NewLayout of (UCStream.location * uc_string * skip_arg * skip_arg)
|  `NewArea of (UCStream.location * uc_string * skip_arg * skip_arg * skip_arg * skip_arg * skip_arg * skip_arg * area_contents_arg)
|  `ShipOut of (UCStream.location * uc_string * uc_string * int)
|  `AddToGalley of (UCStream.location * uc_string * list node_type)
|  `PutGalleyInVBox of (UCStream.location * bool * uc_string)
|  `ModifyGalleyGlue of (UCStream.location * environment -> list box -> list box)
|  `Paragraph of (UCStream.location * list node_type)
|  `BeginGroup of UCStream.location
|  `EndGroup of UCStream.location
|  `Float of (UCStream.location * uc_string * list node_type)
|  `Glyph of (UCStream.location * int)
|  `Letter of (UCStream.location * uc_char)
|  `Space of UCStream.location
|  `Glue of (UCStream.location * dim_arg * dim_arg * bool * bool)
|  `Break of (UCStream.location * option num * bool * list node_type * list node_type * list node_type)
|  `Rule of (UCStream.location * dim_arg * dim_arg * dim_arg)
|  `Image of (UCStream.location * string * skip_arg * skip_arg)
|  `Accent of (UCStream.location * uc_char * list node_type)
|  `HBox of (UCStream.location * list node_type)
|  `HBoxTo of (UCStream.location * skip_arg * list node_type)
|  `HBoxSpread of (UCStream.location * skip_arg * list node_type)
|  `VBox of (UCStream.location * list node_type)
|  `VBoxTo of (UCStream.location * skip_arg * list node_type)
|  `VBoxSpread of (UCStream.location * skip_arg * list node_type)
|  `Phantom of (UCStream.location * bool * bool * list node_type)
|  `HLeaders of (UCStream.location * dim_arg * list node_type)
|  `VInsert of (UCStream.location * bool * list node_type)
|  `Table of (UCStream.location * list node_type)
|  `TableEntry of (UCStream.location * int * int * int * int * int * list node_type)
|  `Math of (UCStream.location * list node_type)
|  `MathCode of (UCStream.location * math_code * list node_type)
|  `MathChar of (UCStream.location * (math_code * (int * int) * (uc_char * uc_char)))
|  `SubScript of (UCStream.location * list node_type)
|  `SuperScript of (UCStream.location * list node_type)
|  `Fraction of (UCStream.location * list node_type * list node_type * node_type * node_type * skip_arg)
|  `Underline of (UCStream.location * list node_type)
|  `Overline of (UCStream.location * list node_type)
|  `MathAccent of (UCStream.location * int * uc_char * list node_type)
|  `Root of (UCStream.location * int * uc_char * int * uc_char * list node_type)
|  `LeftRight of (UCStream.location * list node_type * list node_type * list node_type)
|  `MathStyle of (UCStream.location * MathLayout.math_style)
|  `IndexPosition of (UCStream.location * Box.index_position)
];

value const_pt        : num -> skip_arg;
value const_em        : num -> skip_arg;
value const_ex        : num -> skip_arg;
value const_mu        : num -> skip_arg;
value const_fixed_dim : skip_arg -> dim_arg;

value eval_node_list : environment -> Builder.builder_interface -> list node_type -> environment;
value evaluate       : list node_type -> list FontMetric.page;

