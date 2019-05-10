(* ::Package:: *)

(* ::Chapter::Closed:: *)
(*Begin*)


BeginPackage["Creo`"];

  Needs @ "Meh`";


BeginPackage["`Kernel`"];

BeginPackage["`FrontEndUtilities`"];
EndPackage[];


CreoIcons;


restartKernel::usage = "restartKernel[procedure to evaluate after restart]...";
inputfieldSetFocus::usage = "inputfieldSetFocus[tag_String] sets focus on InputField[.... , BoxID->tag]. "<>
    "The key is to use it good moment. InputField has to be already displayed. One good way is to use CellDynamicExpression etc.";
labeledPanel::usage = "labeledPanel[label, content, contentOptions___]";

selectCells::usage = "selectCells[{__CellObject}] allows to select non adjacent cells. Modifies selection.";
withProgressIndicator::usage = "withProgressIndicator[proc, delay:0] runs the proc and if it take more than delay,
		 it will prompt a dialog with progress indicator and Abort[] button. Because of dialog it should be run on main link
		 (Method->\"Queued\" for Button or ActionMenu)";
OpenCloseAll::usage = "OpenCloseAll[nbObj, cellStyle, Opened/Close] openes/closes groups in given nbObj,
		 which parents are cellStyles.";

QueuedDynamicModule;
QueuedButton;
QueuedController;

CenterToParent;
QueuedActionMenu;

boxInputField; makeInputFormBoxes; toInputFormBoxes;inputFieldFrame;


TableWith

HeaderOpenerView

AssociationInterpretation;

AssociationDynamicModule;
LinkHand;
SuppressReturn;
AutoClose;

DatasetTable;


CreoInputField;
CreoCheckbox;
CreoActionMenu;
CreoPopupMenu;

CreoChoiceDialog;
CreoDialogInput;
CreoDialogPane;
CreoButton; CreoQueuedButton;
CreoProgressIndicator;

EndPackage[];



Begin["`FrontEndUtilities`Private`"];


(* ::Chapter:: *)
(*Independent solutions*)


(* ::Subsection::Closed:: *)
(*TableWith*)


TableWith // Attributes = {HoldAll}
TableWith[body_, spec:{iterator_Symbol, __}]:= Table[
  With[{iterator = iterator}, body]
, spec
];
TableWith[arg___]:=Table[arg]  


(* ::Subsection::Closed:: *)
(*HeaderOpenerView*)


HeaderOpenerView // Options = {
  "Wrapper" -> (Column[{##}, Left]&)
};

HeaderOpenerView[
  {header_, content__}
, Dynamic[state_]
, patt: OptionsPattern[{HeaderOpenerView, OpenerView}]
]:= PaneSelector[
  { False -> header
  , True -> OptionValue["Wrapper"][header, content]
  }
, Dynamic[state]
, FilterRules[{patt}, Options[OpenerView]]
, ImageSize -> Automatic
]  


(* ::Subsection::Closed:: *)
(*DynamicModule - Interpretation for Associations*)


AssociationInterpretation[asso_Association, panel_, head_:Creo`QuestionTemplate]:= With[
  { spec = ToExpression["DM`" <> #, StandardForm, Function[sym, Hold[sym = asso[#]] , HoldAll]]& /@ Keys @ asso
    , out = ToExpression["DM`" <> #, StandardForm, Function[sym, Hold[# -> sym] , HoldAll]]& /@ Keys @ asso
  }
  , Interpretation @@ {
    Unevaluated @@ List @@@ Hold[ Evaluate[ Join @@ spec ] ]
    , panel
    , Unevaluated @@ head /@ Association @@@ Hold[ Evaluate[Join@@out] ]
  }
];


(* ::Subsection::Closed:: *)
(*DynamicModule - pure for Associations*)


AssociationDynamicModule::usage = "the first argument should be obj_Symbol[spec_Association] which will be dynamically assembled while "<>
    "all keys of spec are available in parent DynamicModule as DM`{keyname}$$ (typeset access only). ";

AssociationDynamicModule//ClearAll;
AssociationDynamicModule // Attributes = {HoldFirst};
(*obj value should match head[_Association]*)
AssociationDynamicModule[obj_, panel_]:= Module[{asso = First @ obj, dmwrapper,assembler,dmSpec}, With[
  { spec = ToExpression["DM`" <> #, StandardForm, Function[sym, Hold[sym = asso[#]] , HoldAll]]& /@ Keys @ asso
    , out = ToExpression["DM`" <> #, StandardForm, Function[sym, Hold[# -> sym] , HoldAll]]& /@ Keys @ asso
  },
  dmSpec = List @@@ Hold[ Evaluate[ Join @@ spec ] ];
  assembler=  Head[obj] /@ Association @@@ Hold[ Evaluate[Join@@out] ];
  dmwrapper = assembler /. Hold[content_] :> DynamicModule[
    {DM`parentAssociation}
  , DynamicWrapper[  panel, DM`parentAssociation = First[obj = content]]
  ];

  DynamicModule @@ {
    Unevaluated @@ dmSpec
    , dmwrapper
  }
]];


(* ::Subsection::Closed:: *)
(*console Print*)


NbConsoleMessage[args__] := Module[{messageBoxData},
  messageBoxData =
      Catch[Block[{MessagePacket = Throw[#3, "BeforeLinkWrite"] &},
        Message[args]], "BeforeLinkWrite"]
  ; SelectionMove[MessagesNotebook[], After, Notebook]
  ; NotebookWrite[MessagesNotebook[],
    Cell[messageBoxData, "MessagesWindowMessage", "MSG"]]
]


(* ::Subsection::Closed:: *)
(*queued DynamicModule*)


QueuedDynamicModule::usage = "Heavy duty DynamicModule, shows a progress indicator till the initialization is finished";
QueuedDynamicModule // Attributes = {HoldAll};
QueuedDynamicModule[{spec___}, body_]:=DynamicModule[{spec, initializationDone = False, display, initializationRun}
  , Dynamic[
    If[
      Not @ TrueQ @ initializationDone
      , ProgressIndicator[Appearance -> "Necklace"]
      , display
    ]
    , TrackedSymbols :> {initializationDone}
  ]
  , SynchronousInitialization -> False
  , Initialization :> (
    initializationRun[]:=(
      initializationDone = False
      ; display = body
      ; initializationDone = True
    )
    ; initializationRun[]
  )
];



(* ::Subsection:: *)
(*queued Button*)


QueuedButton::usage = "Heavy duty Button, it will show a progress indicator till the procedure is completed.";
QueuedButton // Attributes = HoldRest;
QueuedButton // Options = {
  CachedValue -> Automatic
  , "ActionDelay" -> 0
};

QueuedButton[lbl_, action_, patt:OptionsPattern[{QueuedButton, Button}]]:= With[
  { temp = OptionValue[CachedValue] /. Automatic -> ProgressIndicator[Appearance -> "Necklace"]
    , delay = OptionValue["ActionDelay"]
  }
  , DynamicModule[{done = True}
    , PaneSelector[
      { True ->Button[lbl
        , done = False; Pause[delay]; CheckAbort[action, $Aborted]; done = True;
        , Method -> "Queued"
        , FilterRules[{patt}, Options[Button]]
      ]
        , False -> temp
      }
      , Dynamic[ done + 0 ]
      , ImageSize -> All
      , Alignment->{Center, Center}
    ]
  ]
];


(* ::Subsection::Closed:: *)
(*queued ActionMenu*)


QueuedActionMenu::usage = "Heavy duty ActionMenu, it will show a progress indicator till the procedure is completed.";

QueuedActionMenu[lbl_, actions_, patt : OptionsPattern[Button]] := DynamicModule[
  {done = True, wrapper}
  ,	wrapper =	Function[expr
    , done = False; CheckAbort[expr, $Aborted];	done = True
    , HoldFirst
  ]
  ;	Dynamic[
    If[
      done
      , #
      , ProgressIndicator[Appearance -> "Necklace"]
    ]
  ]& @ ActionMenu[	lbl
    , actions /. Verbatim[RuleDelayed][label_, action_] :>
        RuleDelayed[label, wrapper[action]]
    , Method -> "Queued"
    , patt
  ]
];



(* ::Subsection::Closed:: *)
(*queued Controller*)


QueuedController // Attributes = {HoldRest};
QueuedController[controller_, action_]:=With[{f := QueuedController`finished}, DynamicModule[
  {f = True}
  , DynamicWrapper[
    Dynamic[If[TrueQ @ f, controller, ProgressIndicator[Appearance->"Percolate"]]]
    , If[ Not @ f, action; f = True ]
    , SynchronousUpdating->False
    , TrackedSymbols:>{f}
  ]
]];




(* ::Subsection::Closed:: *)
(*centered Dialog*)


CenterToParent::usage = "CenterToParent[DialogInput[...]] etc, will make the dialog centered with respect to the parent notebook";

CenterToParent // Attributes = {HoldFirst};
CenterToParent[ dialog_[whatever__, opts : OptionsPattern[]]  ] := With[
  {apc = AbsoluteCurrentValue}
  , With[
    { parentCenter = Transpose@{apc[WindowMargins][[;; , 1]] + .5 apc[ WindowSize], {Automatic, Automatic}}
    }
    , dialog[whatever
      , NotebookDynamicExpression :> Refresh[
        SetOptions[EvaluationNotebook[]
          , WindowMargins -> (parentCenter - Transpose[{.5 apc[WindowSize], {0, 0}}])
          , NotebookDynamicExpression -> None
        ]
        , None
      ]
      , opts ]
  ]
];

CenterToParent[ nb_  ] := With[
  {apc = AbsoluteCurrentValue}
  , With[
      { parentCenter = Transpose@{apc[WindowMargins][[;; , 1]] + .5 apc[ WindowSize], {Automatic, Automatic}}
      }
    , SetOptions[nb, WindowMargins -> (parentCenter - Transpose[{.5 apc[nb, WindowSize], {0, 0}}])]
    ] 
]


(* ::Subsection::Closed:: *)
(*auto close notebook*)


AutoClose[delay_:1][nb_NotebookObject]:=RunScheduledTask[NotebookClose[nb], {delay}];


(* ::Subsection::Closed:: *)
(*InputField utilities*)


SetAttributes[inputfieldSetFocus, HoldRest];

inputfieldSetFocus[tag_String, nb_:EvaluationNotebook[]]:= MathLink`CallFrontEnd[
  FrontEnd`BoxReferenceFind[
    FE`BoxReference[nb, {{tag}}, FE`BoxOffset -> {FE`BoxChild[1]}, FE`SearchStart -> "StartFromBeginning"]
  ]
];




SuppressReturn = EventHandler[#
  , { {"MenuCommand", "HandleShiftReturn"}	:> {}
    , "ReturnKeyDown" 						:> Paste["\n"]
    , {"MenuCommand", "EvaluateCells"}    	:> Paste["\n"]
  }
] &;




(* ::Subsection::Closed:: *)
(*Fast dataset formatting*)


DatasetTable[asso_Association]:= DatasetTable[ Normal @ asso];

DatasetTable[rules:{__?OptionQ}] := Grid[ List @@@ rules
  , Alignment->Left
  , Frame->All
  , FrameStyle->Gray
  , Background->{{LightGray,Inherited}}
  , BaseStyle -> "CreoSmall"
  , Spacings->{1, 1}
];



(* ::Subsection::Closed:: *)
(*Misc*)


LinkHand = MouseAppearance[#, "LinkHand"]&;




labeledPanel = Overlay[{
  Panel[##2, ImageMargins -> {{0, 0}, {0, 15}}],
  Panel[#, ImageMargins -> {{10, 0}, {0, 0}}, Alignment -> Center, FrameMargins -> {{5, 5}, {1, 1}}]
},
  All,
  1
] &;





SetAttributes[restartKernel, HoldAll];

Options[restartKernel] = {"Print" -> dPrint};

restartKernel[after_, OptionsPattern[]] := With[{
  currentSetting = CurrentValue[$FrontEnd, "ClearEvaluationQueueOnKernelQuit"],
  print = OptionValue["Print"]
},
  Composition[
    (SelectionMove[#, All, Notebook]; SelectionEvaluate[#]) &
    ,
    CreateDocument[#, Visible -> True, ImageSize->{400, 100}] &
    ,
    Map[Cell[#, "Input"] &, #] &
    ,
    Map[BoxData, #] &

  ]@{
    MakeBoxes[
      CurrentValue[$FrontEndSession, "ClearEvaluationQueueOnKernelQuit"] = False;
      print["session:", $SessionID]]
    ,
    MakeBoxes[
      print["PrepareToQuit..."]; Quit[];]
    ,
    MakeBoxes[
      print["session:", $SessionID];
      after;
      CurrentValue[$FrontEndSession, "ClearEvaluationQueueOnKernelQuit"] = currentSetting;
      NotebookClose[EvaluationNotebook[]];
    ]
  }
];




SetAttributes[selectCells, HoldRest];

selectCells[cells_, nb_: InputNotebook[], type_:Cell] := With[{tag = "tempCellTag&"},
  (
    SelectionMove[#, All, type, AutoScroll -> False];
    FrontEndExecute@FrontEnd`SelectionAddCellTags[nb, tag]
  ) & /@ cells;

  NotebookLocate[tag];
  FrontEndExecute@FrontEnd`SelectionRemoveCellTags[nb, tag]

];



(*http://mathematica.stackexchange.com/a/102575/5478*)
OpenCloseAll[nb_, target_, what_] := Do[
  SelectionMove[cell, All, CellGroup, AutoScroll -> False];
  With[{
    content = NotebookRead[nb],
    notWhat = what /. {Closed -> Open, Open -> Closed}
  },
    If[
      MatchQ[
        content,
        Cell[CellGroupData[{Cell[_, target, ___], __}, notWhat]]
      ],
      NotebookWrite[nb,
        Cell[CellGroupData[	fixUndocumentedOptions[ content[[1, 1]] ], what]]
        ,
        AutoScroll -> False
      ]
    ]
  ];
  ,
  {cell, Cells[nb, CellStyle -> target]}
];

fixUndocumentedOptions[expr_]:= ReplaceAll[
  expr,
  s_Symbol /; Context[s] === "Global`" :> Symbol["FrontEnd`" <> SymbolName[s]]
];



boxInputField::usage = "works as an ordinary Boxes InputField with a styling known from Input cells";

boxInputField[x_, opts:OptionsPattern[InputField] ]:= Module[{baseStyle},


  baseStyle = Join[
    {"Notebook", "Input", FormatType -> InputForm},
    (# -> CurrentValue[{"StyleDefinitions", "Input", #}] & /@ DeleteCases[
      Keys@CurrentValue[{"StyleDefinitions", "InputField"}], "ContextMenu"]
    )
  ];

  inputFieldFrame[Creo`adarkpink] @ InputField[x, Boxes,
    BaseStyle -> baseStyle, opts,
    FrameMargins -> 0, Appearance -> "Frameless"


  ]
];

inputFieldFrame[color_, opts___]:= Framed[
  #, FrameStyle -> color, opts, RoundingRadius -> 0, BoxFrame -> 4,  FrameMargins -> 0
]&;


SetAttributes[makeInputFormBoxes, HoldAllComplete];

makeInputFormBoxes::usage = "gives an input form boxes without interpretation rules defined for head.";

makeInputFormBoxes[expr_] := Module[{str}
  , str = ToString[Unevaluated[expr], InputForm]
  ; UsingFrontEnd @ MathLink`CallFrontEnd[ (*UsingFrontEnd - for cloud evaluations*)
    FrontEnd`UndocumentedTestFEParserPacket[str, True]
  ][[1, 1]]
];

toInputFormBoxes::usage = makeInputFormBoxes::usage;
toInputFormBoxes[expr_] := makeInputFormBoxes@expr;



(* ::Chapter:: *)
(*CREO UI*)


(* ::Subsection::Closed:: *)
(*icons*)


iconsPath[]:= FileNameJoin[{Creo`$CreoProjectDir, "Files", "TextResources", "icons.tr"}]

If[
  $InputFileName =!= "" (*from package or from navigator initialization?*)
  , CreoIcons = #[[{1}]]& (*1*) /@ Uncompress @ Get @ iconsPath[]
; dPrint["icons loaded"]
  , dPrint["icons inherited"]
];

CreoIcons // AssociationQ // Replace[False :> Message[Get::noopen, "CreoIcons"]];

(*1 done to drop options from graphics, especially padding
  e.g. Graphics[Disk[], ImageMargins\[Rule]10][[{1}]]//InputForm
 *)


(* ::Subsection::Closed:: *)
(*InputField*)


CreoInputField // Options = {
  Underlined -> True
, BoxID -> "CreoInputField"
};
CreoInputField[Dynamic[spec__], type_, options:OptionsPattern[{CreoInputField, InputField}]]:= Grid[
  {{ InputField[ Dynamic[spec], type
     , Sequence @@ FilterRules[{options}, Except[Underlined]]
     , Background->None
     , Appearance->None
     , BaseStyle->"CreoBasic"
     ]
  }}
  , Dividers -> If[TrueQ @ OptionValue @ Underlined, {None,None,{-1->Thick}}, None]
  , BaselinePosition-> {{1,1}, Baseline}
  , Spacings -> {1,0}
  ]


(* ::Subsection::Closed:: *)
(*PopupMenu ActionMenu*)


CreoMenuWrapper[menu_]:=Grid[
  { 
    { menu
    , Pane[CreoIcons["ico.drop"],ImageSize->{Automatic, .7FrontEnd`CurrentValue["FontSize"]}, BaseStyle->"CreoBasic"]
    }
  
  }
  , Alignment->{Center,Center}
  , BaselinePosition->{{1,1},Baseline}
  , Dividers->{None,None,{-1->Thick}}
  , Spacings -> {1, .1}
];


CreoActionMenu[label_, spec_, options___]:=  ActionMenu[
    CreoMenuWrapper @ label
  , spec
  , options
  , Appearance->None
  , ImageSize -> {{170, All}, {Automatic, Automatic}}
  , Alignment->{Left, Center}
  , BaseStyle->"CreoBasic"
  , MenuStyle->"CreoBasic"
]



CreoPopupMenu[dyn:Dynamic[var_, dynSpec___], spec:{__Rule}, options___?OptionQ]:= CreoPopupMenu[
  dyn
, spec
, ""
, CreoMenuWrapper[Dynamic[var /. Append[spec, _ -> "choose..."]]]
, options
]

CreoPopupMenu[dyn : Dynamic[var_, dynSpec___], spec_List, options___?OptionQ]:= CreoPopupMenu[
  dyn
, spec
, ""
, CreoMenuWrapper[Dynamic[var /. Except[Alternatives@@spec] -> "choose..."]]
, options
]


CreoPopupMenu[varSpec_Dynamic, spec_, def_, dbase_, options___]:= PopupMenu[
    varSpec
  , spec
  , def
  , dbase
  , options
  , Appearance->None
  , ImageSize -> {{170, All}, {Automatic, Automatic}}
  , Alignment->{Left, Center}
  , BaseStyle->"CreoBasic"
  , MenuStyle->"CreoBasic"
]


(* ::Subsection::Closed:: *)
(*Checkbox*)


CreoCheckbox[Dynamic[var_, action_:Automatic], spec:Except[_?OptionQ]:{True, False} , opts:OptionsPattern[]]:= With[{}, LinkHand[
  EventHandler[
    PaneSelector[
      { First @ spec ->  CreoIcons["ico.switch.on"]
        , Last @ spec ->  CreoIcons["ico.switch.off"]
      }
      , Dynamic[ var ]
      , opts
      , ImageSize -> {Automatic, (*1.5*) FrontEnd`CurrentValue["FontSize"]}
    ]
    , { "MouseClicked" :> Module[
    { val = var /. Thread[spec -> Reverse@spec]
      , func = action /. Automatic -> Function[tf, var = tf]
    }
    , func[val]
    ; QueuedController`finished$$ = False
  ]
  }

  ]
]]




(* ::Subsection:: *)
(*Button*)


CreoButton // Attributes = {HoldRest};

CreoButton // Options = {
  "Theme" -> Automatic
};

CreoButton[lbl_, action_, opts:OptionsPattern[{CreoButton, Button}]]:= With[
  { baseBg = Switch[ OptionValue["Theme"]
    , Automatic, GrayLevel[213/255.]
    , "Dark",    GrayLevel[190/255.]
    , _,         OptionValue["Theme"]
    ]
  , hoverBg = Switch[ OptionValue["Theme"]
      , Automatic, GrayLevel[1.]
      , "Dark",    GrayLevel[.5]
      , _,         Lighter @ OptionValue["Theme"]
    ]
  }
  , Button[
    lbl // Replace[s_String :> ToUpperCase[s]]
    , action
    , FilterRules[{opts}, Options @ Button]
    , Appearance -> None
    , Background -> Dynamic[ FEPrivate`If[
      CurrentValue["MouseOver"]
      , hoverBg
      , baseBg]
    ]
    , BaseStyle -> {"CreoBasic", LineBreakWithin -> False}
    , FrameMargins -> {{10,10},{5,5}}
    , ImageSize->{{110, Full}, 30}
  ]
];


CreoQueuedButton // Attributes = {HoldRest};
CreoQueuedButton[lbl_, action_, opts___]:= QueuedController[
  CreoButton[lbl, QueuedController`finished$$=False, opts]
  , action
]


(* ::Subsection::Closed:: *)
(*Dialogs common*)


CreoDialogPane = Pane[##
  , ImageSize -> {{280, Scaled @ 1}, {All, Scaled @ 1}}
  , ImageMargins -> 15
  , FrameMargins -> 0
  , Alignment -> {Center,Center}
  , BaseStyle -> "CreoBasic"

]&;


(* ::Subsection::Closed:: *)
(*DialogInput variations*)


CreoDialogInput // Attributes = {HoldAll};

CreoDialogInput // Options = {
  "LabelFunction" -> Identity
  , "Title"         -> ""
  , "Ok"            -> "Ok"
  , "Cancel"        -> "Cancel"
  , "CancelReturns" -> $Canceled (*can be rule delayed based on vars*)
  , ControlType     -> CreoPopupMenu
};

CreoDialogInput[
  vars_, panel_, return: Except[_?OptionQ]
  , OptionsPattern[{CreoDialogInput, CreateDialog}]
]:=With[
  { cancelReturns = OptionValue[Automatic, Automatic, "CancelReturns", Hold]
    , ok = OptionValue["Ok"]
    , cancel = OptionValue["Cancel"]
    , title = OptionValue["Title"]
  }
  , CenterToParent @ DialogInput[
    vars
    , CreoDialogPane @ Column[
      {panel
        , Grid[{{
        CreoButton[ok, DialogReturn[return]]
        , CreoButton[cancel, DialogReturn @ ReleaseHold @ cancelReturns]
      }}]
      }
      , Center
      , Spacings -> 2
    ]
    , StyleDefinitions -> "CreoDialog.nb"
    , TaggingRules -> {"dialogHeader" -> title}
  ]
];



CreoDialogInput[
  lbl_, choices_List
  , opts:OptionsPattern[{CreoDialogInput, CreateDialog}]
]:= CreoDialogInput[
  {choice = First @ choices}
  , Column[{
    lbl
    , OptionValue[ControlType][
      Dynamic@choice
      , OptionValue["LabelFunction"] /@ choices
    ]
  }, Spacings->1]
  , choice
  , opts
]


CreoChoiceDialog[msg_, title_]:= CreoDialogInput[
  {}, msg, True
  , "Ok"->"Yes"
  , "Cancel" -> "No"
  , "CancelReturns" -> False
  , "Title" -> title
];

(*CenterToParent @ DialogInput[
      CreoDialogPane @ Column[{
          msg
        , Grid[{{ 
              CreoButton["Yes",DialogReturn[True]]
            , CreoButton["No",DialogReturn[$Canceled]]
         }}]
      }, Right, Spacings -> 2]
    , StyleDefinitions -> "CreoDialog.nb"
    , TaggingRules -> {"dialogHeader" -> title}
]*)


CreoDialogInput[title_, vars_, panel_, return_]


(* ::Subsection::Closed:: *)
(*progress Dialog*)


ClearAll @ withProgressIndicator;
SetAttributes[withProgressIndicator, HoldFirst];

(*there are rumors it is not so stable on Unix as Patric says but it's something*)

withProgressIndicator[proc_, message_, delay : _?NonNegative : 1] := Module[
  {dialog, scheduledTask, dialogPrompt}
  , dialogPrompt[] := (
    dialog = CenterToParent @ CreateDialog[
      CreoDialogPane @ Column[
        { message
          , CreoProgressIndicator["Indeterminate"]
          , CreoButton[
          "Abort"
          , FrontEndExecute[FrontEndToken["EvaluatorAbort"]]
          ; DialogReturn[]
          , Method -> "Preemptive"
        ]
        }
        , Center, Spacings -> 1
      ]
      , WindowFloating -> True
      , WindowFrameElements -> None
      , TaggingRules -> {"dialogHeader" -> "Processing"}
      , StyleDefinitions -> "CreoDialog.nb"
    ]
  )

   
  ; Internal`WithLocalSettings[
      scheduledTask = RunScheduledTask[dialogPrompt[], {delay}]
    , CheckAbort[proc, $Aborted]
    , RemoveScheduledTask@scheduledTask
    ; NotebookClose[dialog]
    ]
];



CreoProgressIndicator["Indeterminate", opts___]:= DynamicModule[
  {t=0}
  , Grid[{{
    Animator[Dynamic@t,{0,1.33},AppearanceElements->{},DefaultDuration->2,ImageSize->{1,1}]
    , Graphics[{GrayLevel@.8, Rectangle[],Black ,Rectangle[{Dynamic[t-.33],0},{Dynamic[t],1}]}
      , opts
      , ImageSize->280
      , AspectRatio->.04
      , PlotRange->{{0,1},{0,1}}
      , PlotRangePadding->Scaled[.01]
    ]
  }}, Spacings->{0,0}, BaseStyle -> CacheGraphics-> False
  ]
];


(* ::Chapter:: *)
(*End*)


End[];


EndPackage[];
