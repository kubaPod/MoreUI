
BeginPackage["MoreUI`"]

	ActionNestedMenu

Begin["`Private`"]

	actionNestedMenu[menuLabel_ -> spec_] := DynamicModule[{menu},
  parseSpec[menu, spec, 1];
  
  Button[menuLabel, Appearance -> "DefaultButton"]
  ]

End[];	
	
EndPackage[];