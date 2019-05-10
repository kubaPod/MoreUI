
BeginPackage["MoreUI`"]

	ActionNestedMenu;
  SearchPopup;
  
  ImagePane;
  PlotRangeSelector;
  QueuedController;
  BusyButton;

Begin["`Private`"]



BusyButton::usage = "Heavy duty Button, it will show a progress indicator till the procedure is completed.";
BusyButton // Attributes = HoldRest;
BusyButton // Options = {
  CachedValue -> Automatic
  , "ActionDelay" -> 0
};

BusyButton[lbl_, action_, patt:OptionsPattern[{BusyButton, Button}]]:= With[
  {   temp = OptionValue[CachedValue] /. Automatic -> ProgressIndicator[Appearance -> "Necklace"]
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


QueuedController // Attributes = {HoldRest};

QueuedController::usage = "QueuedController[controller, action] will execute action after dragging of controller is done.";

QueuedController[
  controller_[Dynamic[var_, dragF_:Automatic], rest___],
  action_
]:=DynamicModule[
  { finished = True
    , temp = var
    , onDrag = dragF
  }
  , DynamicWrapper[#, temp = var, Evaluate @ ToTrackedSymbol @ var]& @
  DynamicWrapper[
    controller[Dynamic[ temp, {onDrag, (finished = False)&}], rest ]
    , If[ Not @ finished, action @ temp; finished = True]
    , SynchronousUpdating->False
    , TrackedSymbols:>{finished}
  ]
]


ToTrackedSymbol // Attributes = {HoldAll};
ToTrackedSymbol[ s_Symbol ]:= TrackedSymbols :> {s};
ToTrackedSymbol[ (s_Symbol)[___] ]:= TrackedSymbols :> {s};
ToTrackedSymbol[ ___ ]:= TrackedSymbols :> Full

ImagePane::usage = "ImagePane[Dynamic[imgSource], graphicsOverlay] displays resized image to spare FrontEnd dealing with whatever huge image could be.";

ImagePane[Dynamic[image_], realPrimitives_:{} ]:= DynamicModule[
  { realDim, realAspectRatio, viewWidth, viewDim, plotRange
    , $$shift, $$scaling, $$cropRange, $$imageView, crop, init, resetCrop
  }
  
  , init[]:= (
      realDim = N @ ImageDimensions @ image
    ; realAspectRatio = #2/# & @@ realDim
    ; viewWidth = 500.
    ; viewDim = viewWidth realDim / realDim[[1]]
    ; plotRange = Transpose @ {{0,0},viewDim}
  )
  
  
  ; crop[ranges:{{xMin_, xMax_},{yMin_, yMax_}}]:=Module[
    {temp}
    , temp = ranges
    ; temp[[2, 2]] = yMin + (xMax - xMin) realAspectRatio
    ; temp = Transpose @ temp
    
    ; temp =( # / $$scaling + $$shift) & /@ temp
    
    ; $$cropRange = Transpose @ temp
    ; $$shift = $$cropRange[[All,1]]
    ; $$scaling = viewWidth / ($$cropRange[[1,2]]-$$cropRange[[1,1]])
    
    ; $$imageView = ImageResize[
      ImageTrim[image, Transpose @ $$cropRange]
      , viewWidth
      , Resampling -> If[$$scaling > 1, "Nearest", Automatic]
    ]
  ]
  
  ; resetCrop[]:= (
    
      $$shift = {0,0}
    ; $$scaling = viewWidth / realDim[[1]]
    ; crop @ Transpose @ {{0,0}, viewDim }
  )
  
  ; init[]
  ; resetCrop[]
  
  ; DynamicWrapper[
    #
    , $$imageView = ImageResize[ImageTrim[image, Transpose @ $$cropRange], viewWidth, Resampling -> If[$$scaling > 1, "Nearest", Automatic]]
    , TrackedSymbols :> {image}
  ]& @
  PlotRangeSelector[
    Dynamic @ plotRange
    , Graphics[
    { Inset[Dynamic[$$imageView], {0,0}, {0,0}, viewDim]
      , GeometricTransformation[
      realPrimitives
      , Dynamic[ScalingTransform[{1,1} $$scaling]@*TranslationTransform[ - $$shift ]]
    ]
    }
    , ImageSize -> viewDim
    , BaseStyle-> { CacheGraphics -> False }
  ]
    , "on-plotRangeChange" -> crop
    , "on-plotRangeReset" -> resetCrop
    , AspectRatio -> realAspectRatio
  ]
]


PlotRangeSelector::usage = "PlotRangeSelector[graphics] is an EventHandler that allows user to modify scene plot range with dragged rectangle.";

PlotRangeSelector // Options = {
  "on-plotRangeChange" -> Automatic
  , "on-plotRangeReset"  -> Automatic
  , AspectRatio          -> Automatic
}

PlotRangeSelector[graphics_Graphics, opt:OptionsPattern[] ]:= DynamicModule[{plotRange = PlotRange @ graphics},
  PlotRangeSelector[Dynamic @ plotRange, graphics, opt]
]

PlotRangeSelector[ Dynamic[plotRange_], graphics_,  OptionsPattern[]]:= DynamicModule[
  { show = False, pos1, pos2, original = plotRange
    , onPlotRangeChange = OptionValue["on-plotRangeChange"] /. Automatic -> Function[val, plotRange = val]
    , onPlotRangeReset = OptionValue["on-plotRangeReset"] /. Automatic -> Function[ plotRange = original]
    , onDrag
  },
  With[
    {  mousePosition := CurrentValue[{MousePosition,"Graphics"}], aspectRatio = OptionValue[AspectRatio]},
    
    onDrag = If[ NumericQ @ aspectRatio
      , Function[pos2 = pos1 + {  #[[1]] ,  Sign[#[[2]]] aspectRatio #[[1]] } &@(mousePosition - pos1) ]
      , Function[pos2 = mousePosition ]
    ]
    
    ; EventHandler[
      Show[
        graphics
        , Graphics[{
        Dynamic@If[show
          , { FaceForm[], EdgeForm@Directive[White, Thin,Dashed], Rectangle[Dynamic@pos1,Dynamic@pos2]}
          , {}
        ]
      }]
        , PlotRange -> Dynamic @ plotRange
      ]
      , { "MouseDown"        :> ( pos1 = mousePosition)
        , "MouseDragged"     :> ( show = True; onDrag[])
        , "MouseUp"          :> If[show, show=False; onPlotRangeChange[ Sort/@Transpose[{pos1,pos2}] ]]
        , {"MouseClicked", 1}:> Null
        , {"MouseClicked", 2}:> ( onPlotRangeReset[] )
      }
    ]
  ]];




  ActionNestedMenu[menuLabel_ -> spec_] := DynamicModule[{},

    (*Grid[{{#, Dynamic[Column@Array[menuStates,{3}],UpdateInterval->1]}}, Alignment->{Left,Top}]&@*)
    subMenuWrapper[
      menuStates,
      0,
      {
        subMenuGate[
          menuStates,
          <|
            "label"             -> menuLabel,
            "subMenuAlignment"  -> {Left, Bottom},
            "subMenu"           -> parseSpec[menuStates, spec, 1]
          |>,
          0
        ]
      }
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

    (************* action label wrapper **************)
  parseSpec[menuStates_, RuleDelayed[lbl_ , action_], n_]:= EventHandler[
    Button[lbl, action; (*optional*) dropSubMenu[menuStates, 1],
      Appearance    -> "Frameless",
      FrameMargins  -> 5,
      ImageMargins  -> 0,
      Alignment     -> Left,
      ImageSize     -> {{120, Full}, {Automatic, Full}},
      Background    -> Dynamic[If[CurrentValue@"MouseOver", GrayLevel@.8, None]]
    ]
    ,
    {
      "MouseEntered" :> (
        dropSubMenu[menuStates, n + 1]
      )
    },
    PassEventsDown -> True
  ];


  SetAttributes[subMenuWrapper, HoldFirst];

  subMenuWrapper[menuStates_, level_, content_]:= EventHandler[
    Framed[
      Column[content],
      FrameStyle->None,
      FrameMargins->0,
      ImageMargins ->0,
      ImageSize->{{120, Full}, {Automatic, Full}}
    ],
    {
      "MouseEntered" :> (
        forgetAboutClosing @ menuStates[-1];

      ),
      "MouseExited" :> (
        menuStates[-1] = scheduleMenuClosing[menuStates, level];
      )
    }
    ,
    PassEventsDown -> True
    ];

  (************* gate item label wrapper **************)
  SetAttributes[subMenuGate, HoldFirst];

  subMenuGate[menuStates_, label_String, level_Integer]:= subMenuGate[
    menuStates,
    <|"label"->label|>,
    level
  ];

  subMenuGate[menuStates_, spec_Association, level_Integer]:= DynamicModule[{thisBox, sumbMenuBox},

    EventHandler[
      Framed[
        Grid[{{ Pane[spec["label"], 100], ">"}}],
        BaseStyle -> "Panel",
        FrameStyle-> If[level == 0, 2, None],
        FrameMargins->5,
        ImageMargins ->0,
        ImageSize->{{120, Full}, {Automatic, Full}},
        Background -> Dynamic[If[CurrentValue@"MouseOver", GrayLevel@.8, None]]
      ]
      ,
      {
        "MouseEntered" :> (

          dropSubMenu[menuStates, level + 1];
          NotebookDelete @ sumbMenuBox;

          menuStates[level + 1] = sumbMenuBox = attachTo[
            thisBox, spec["subMenu"], Lookup[spec, "subMenuAlignment", {Right, Top}]];

        )
      },
      PassEventsDown -> True

    ]
    ,
    Initialization:>(
      thisBox = EvaluationBox[]
    )
  ];



  attachTo[parentbox_, what_, alignment_] := MathLink`CallFrontEnd[
        FrontEnd`AttachCell[
          parentbox,
          ToBoxes[ExpressionCell[
            what,
            StripOnInput      -> True,
            Background        -> CurrentValue@"PanelBackground",
            CellFrame         -> 1,
            CellFrameMargins -> 0
            ]
          ],
          {Automatic, alignment},
          {Left, Top},
          "ClosingActions" -> { "ParentChanged", "EvaluatorQuit"}
        ]
  ];

  SetAttributes[dropSubMenu, HoldFirst];

  dropSubMenu[menuStates_, n_]:= (

    If[
      ValueQ @ menuStates[n]
      ,
      NotebookDelete @ menuStates[n];
      menuStates[n]=.;
    ]
  );

  SetAttributes[forgetAboutClosing, HoldFirst];
  forgetAboutClosing[task_]:=(

    RunScheduledTask[ Quiet @ RemoveScheduledTask @ task, {.1}]

  );


  SetAttributes[scheduleMenuClosing, HoldFirst];

  scheduleMenuClosing[menuStates_, level_]:=RunScheduledTask[
    If[

      True
      ,

      dropSubMenu[menuStates, 1];(*this is enough since ParentChanged is included*)
      menuStates[-1]=.;

    ];
    RemoveScheduledTask[$ScheduledTask];

    ,
    {.5}
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