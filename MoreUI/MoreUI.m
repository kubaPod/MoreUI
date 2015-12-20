
BeginPackage["MoreUI`"]

	ActionNestedMenu;
  SearchPopup;

Begin["`Private`"]

  ActionNestedMenu[menuLabel_ -> spec_] := DynamicModule[{},

    Grid[{{#, Dynamic[Column@Array[menuStates,{3}],UpdateInterval->1]}}, Alignment->{Left,Top}]&@
    subMenuWrapper[menuStates, 1, {#}]&@subMenuGate[
      menuStates,
      <|
        "label"             -> menuLabel,
        "subMenuAlignment"  -> {Left, Bottom},
        "subMenu"           -> parseSpec[menuStates, spec, 1]
      |>,
      0
    ]
  ];


  SetAttributes[parseSpec, HoldFirst];

  parseSpec[menuStates_, spec_List, n_]:= subMenuWrapper[
    menuStates,
    n,
    parseSpec[menuStates, #, n]& /@ spec
  ];

  parseSpec[menuStates_, lbl_ -> subMenu_, n_]:= subMenuGate[
    menuStates,
    <|
        "label"             -> lbl,
        "subMenu"           -> parseSpec[menuStates, subMenu, n+1],
        "subMenuAlignment"  -> {Right, Top}
        |>,
    n
  ];

    (* action label wrapper*)
  parseSpec[menuStates_, lbl_ :> action_, n_]:= EventHandler[
    Button[lbl, action, Appearance -> "DefaultButton"]
    ,
    "MouseEntered" :> (
      Print["-----------actionLabel---------------"];
      forgetAboutClosing @ menuStates[-1];
      dropSubMenu[menuStates, n+1]
    ),
    PassEventsDown -> True
  ];

  SetAttributes[subMenuWrapper, HoldFirst];

  subMenuWrapper[menuStates_, level_, content_]:= EventHandler[
    Framed[
      Column[content],
      FrameStyle->Red, FrameMargins->0, ImageMargins ->0
    ],
    "MouseEntered":>(
      forgetAboutClosing @ menuStates[-1];
      Print["just entered subMenu ", level]

    ),
    "MouseExited" :> (
      Print["Exiting subMenu ",level, " will run menu closing task in .4 sec"];

      menuStates[-1] = scheduleMenuClosing[menuStates, level];
      (*Print["menuStates[-1] - ",Head@menuStates[-1]];*)

    )
    ,

    PassEventsDown -> True
    ];


  SetAttributes[subMenuGate, HoldFirst];

  subMenuGate[menuStates_, label_String, level_Integer]:= subMenuGate[
    menuStates,
    <|"label"->label|>,
    level
  ];

  subMenuGate[menuStates_, spec_Association, level_Integer]:= DynamicModule[{thisBox, sumbMenuBox},

    EventHandler[
      Framed[ spec["label"] ]
      ,
      "MouseEntered" :> (
        Print["-----------subMenuGate---------------"];
        forgetAboutClosing @ menuStates[-1];

        dropSubMenu[menuStates, level+1];
        NotebookDelete @ sumbMenuBox;
        Print["setting", menuStates," ", level+1];
        menuStates[level+1] = sumbMenuBox = attachTo[
          thisBox, spec["subMenu"], Lookup[spec, "subMenuAlignment", {Right,Top}]];

      )
    ]
    ,
    Initialization:>(
      thisBox = EvaluationBox[]
    )
  ];

  (*EventHandler[Panel["floating panel"],*)
    (*"MouseExited" :> (NotebookDelete[*)
      (*ParentCell[EvaluationBox[]]];)]*)

  attachTo[parentbox_, what_, alignment_] := MathLink`CallFrontEnd[
        FrontEnd`AttachCell[
          parentbox,
          ToBoxes[ExpressionCell[
            what,
            StripOnInput      -> True,
            Background        -> CurrentValue@"PanelBackground",
            CellFrameMargins  -> 0
            ]
          ],
          {Automatic, alignment},
          {Left, Top},
          "ClosingActions" -> {
            "ParentChanged", "EvaluatorQuit"}
        ]
  ];

  SetAttributes[dropSubMenu, HoldFirst];

  dropSubMenu[menuStates_, n_]:= (
    Print["droping deeper menu: ",n];
    If[
      ValueQ @ menuStates[n]
      ,
      NotebookDelete @ menuStates[n];
      menuStates[n]=.;
    ]
  );

  forgetAboutClosing[task_ScheduledTaskObject]:=(
    Print["droping scheduled task"];
    Quiet @ RemoveScheduledTask @ task;
  );


  SetAttributes[scheduleMenuClosing, HoldFirst];

  scheduleMenuClosing[menuStates_, level_]:=RunScheduledTask[
    If[
      (*(Print[#];#)& @ Not @ ValueQ @ menuStates[level+1]*)
      True
      ,
      Print["im trying to close menu"];
      dropSubMenu[menuStates, 1];(*this is enough since ParentChanged is included*)
      menuStates[-1]=.;

    ];
    RemoveScheduledTask[$ScheduledTask];

    ,
    {.2}
  ];


  (***********************************************************************************)
  (***********************************************************************************)
  (***********************************************************************************)

  SearchPopup[Dynamic[var_], list_] := DynamicModule[{
    thisBox, currentList = {}, autocompleteF, attachedCell, dynamicFunction,
    whatToDo, lastLength, cPos = 1, itemWrapper, attachTo, dropMenu, menu,
    updateCurrentList, scrollY = 1
    },

    EventHandler[
      InputField[Dynamic[var, dynamicFunction, SynchronousUpdating->False], String,
        FieldHint -> "Search",
        ContinuousAction -> True
      ],
      {"MouseClicked" :> (
        If[ Length@currentList > 0, attachedCell = attachTo[thisBox, menu[currentList]]]
      ),
      "DownArrowKeyDown" :> (
        cPos = Mod[cPos + 1, Length@currentList, 1];


        If[cPos > $menuMaxHeight / $itemHeight, scrollY+= $itemHeight];
        If[cPos == 1, scrollY =1];

      ),
      "UpArrowKeyDown" :> (
        cPos = Mod[cPos - 1, Length@currentList, 1]
      ),
      "ReturnKeyDown" :> (
        updateCurrentList[];
        If[
          Length[currentList] >= cPos
          ,
          var = currentList[[cPos]];
          cPos =1;
          FinishDynamic[];
        ];
        dropMenu[];
      SelectionMove[EvaluationNotebook[], After,Expression]
      )},
      PassEventsDown -> True,
      Method->"Queued"
    ],
    SynchronousInitialization -> False,
    Initialization :> (
      $itemHeight = 25;
      $menuMaxHeight = 300;

      thisBox = EvaluationBox[];
      autocompleteF = Autocomplete[list];
      currentList = autocompleteF@var;

      dynamicFunction = (
        var = #;
        updateCurrentList[];
        whatToDo[]
      ) &;

      whatToDo[] := Which[
        lastLength != 0 && Length[currentList] == 0,
        dropMenu[]
        ,
        Or[
          lastLength == 0 && Length[currentList] > 0,
          And[
            lastLength != 0 && Length[currentList] != 0,
            Not@MatchQ[attachedCell, _CellObject]
          ]
        ],
        attachedCell = attachTo[thisBox, menu[currentList]]
      ];

      attachTo[parentbox_, what_] := MathLink`CallFrontEnd[
        FrontEnd`AttachCell[
          parentbox,
          ToBoxes[ExpressionCell[what,
            StripOnInput -> True,
            Background -> White,
            CellFrameColor -> GrayLevel@.8,
            CellFrameMargins -> 0,
            CellFrame -> 2
          ]],
          {Automatic, {Left, Bottom}},
          {Left, Top},
          "ClosingActions" -> {"ParentChanged", "EvaluatorQuit", "OutsideMouseClick"}]];

        SetAttributes[menu, HoldFirst];
        menu[currentList_] := DynamicModule[{},EventHandler[
          Pane[Dynamic[
            Column[
              MapIndexed[itemWrapper, currentList],
              Spacings -> 0
            ],
            TrackedSymbols :> {currentList}
          ],
            {200, {All, 300}},
            AppearanceElements -> None,
            Scrollbars -> {False, True},
            ScrollPosition -> Dynamic[{0, scrollY}, (scrollY=Last@#)&]
          ],
          "MouseExited" :> (dropMenu[];)
        ],InheritScope->True];

        itemWrapper[item_, {pos_}] := MouseAppearance[#, "LinkHand"] &@ EventHandler[
          Framed[item,
            ImageSize -> {Full, $itemHeight},
            FrameStyle -> None,
            Background -> Dynamic[If[pos == cPos, CurrentValue@"PanelBackground", White]]
          ],
          {"MouseClicked" :> (
            pos == cPos;
            var = item;
            cPos = 1;
            dropMenu[];
            updateCurrentList[];
          ),
          "MouseEntered" :> (cPos = pos)},
          PassEventsDown -> True
        ];

        dropMenu[] := (
        If[MatchQ[attachedCell, _CellObject],
          NotebookDelete[attachedCell];
          attachedCell =.
        ]);

        updateCurrentList[]:=(
          lastLength = Length @ currentList;
          currentList = autocompleteF[var];

        )
    )
  ];


End[];	
	
EndPackage[];