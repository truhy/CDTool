�
 TFORM3 0�  TPF0TForm3Form3Left� TopHWidth5Height�BorderWidthCaptionView CD TOC (Table Of Content)Color	clBtnFaceConstraints.MinHeight�Constraints.MinWidth5Font.CharsetDEFAULT_CHARSET
Font.ColorclWindowTextFont.Height�	Font.NameMS Sans Serif
Font.Style 	FormStylefsStayOnTopOldCreateOrderPositionpoMainFormCenterVisible	OnClose	FormCloseOnCreate
FormCreate	OnDestroyFormDestroy
DesignSize#| PixelsPerInch`
TextHeight TLabelMCN_LblLeft Top� Width� HeightCaptionMCN (Media Catalogue No.) :  TLabel
Device_LblLeft TopWidthRHeightCaptionDevice selection:  TLabelLabel1Left TopFWidthVHeightCaptionRead TOC format:  TLabelLabel2Left Top� Width7HeightCaptionSession no:  	TComboBox	CD_dev_CBLeft Top(Width#HeightStylecsDropDownListAnchorsakLeftakTopakRight 
ItemHeightTabOrder OnChangeCD_dev_CBChange  TButton	RescanBtnLeft�TopWidthYHeightAnchorsakTopakRight CaptionRescanTabOrderOnClickRescanBtnClick  TButtonRereadTOCBtnLeft�TopwWidthYHeightAnchorsakTopakRight CaptionRead TOCTabOrderOnClickRereadTOCBtnClick  TEditMCN_EditLeft� Top� Width�HeightBorderStylebsNoneColorclInactiveBorderCtl3D	ParentCtl3DReadOnly	TabOrder  	TCheckBox	MCN_CheckLeft TopxWidthiHeightCaptionRead MCNColorclInactiveBorderParentColorTabOrderOnClickMCN_CheckClick  TStringGridTOC_StringGridLeft Top� Width#Height� AlignalCustomAnchorsakLeftakTopakRightakBottom DefaultColWidth� DefaultRowHeight	FixedCols RowCountOptionsgoFixedVertLinegoFixedHorzLine
goVertLine
goHorzLinegoRangeSelectgoDrawFocusSelectedgoColSizinggoRowSelectgoThumbTracking TabOrder  	TCheckBoxRead_ISRC_CheckLeftxTopxWidthiHeightCaption	Read ISRCColorclInactiveBorderParentColorTabOrderOnClickRead_ISRC_CheckClick  TButtonPrevSess_BtnLeftpTop� WidthHeightCaption<TabOrderTabStopOnClickPrevSess_BtnClick  TEdit
ED_sess_noLeft Top� WidthaHeightAutoSizeFont.CharsetDEFAULT_CHARSET
Font.ColorclWindowTextFont.Height�	Font.NameMS Sans Serif
Font.Style 	MaxLength
ParentFontTabOrderOnExitED_sess_noExit
OnKeyPressED_sess_noKeyPress  TButtonNextSess_BtnLeft� Top� WidthHeightCaption>TabOrder	OnClickNextSess_BtnClick  	TComboBoxCB_read_TOC_modeLeft TopXWidth#HeightStylecsDropDownListAnchorsakLeftakTopakRight 
ItemHeight	ItemIndex TabOrder
Text,All complete sessions TOC info (Code: 0000b)OnChangeCB_read_TOC_modeChangeItems.Strings,All complete sessions TOC info (Code: 0000b),Last complete session TOC info (Code: 0001b)1Full TOC info (selectable sessions) (Code: 0010b)   	TCheckBoxCHK_disp_raw_QLeft0Top� Width� HeightCaptionDisplay in raw Q formatColorclInactiveBorderParentColorTabOrderOnClickCHK_disp_raw_QClick  	TCheckBoxCHK_disp_all_sessLeft� Top� WidthqHeightCaptionDisplay all sessionsColorclInactiveBorderParentColorTabOrderOnClickCHK_disp_all_sessClick  	TCheckBoxCHK_read_CD_modeLeft� TopxWidthaHeightCaptionRead CD modeColorclInactiveBorderParentColorTabOrderOnClickCHK_read_CD_modeClick   