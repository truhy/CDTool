�
 TFORM2 0  TPF0TForm2Form2Left� Top]Width5Height�BorderWidthCaptionView sectorsColor	clBtnFaceConstraints.MinHeight�Constraints.MinWidth5Font.CharsetDEFAULT_CHARSET
Font.ColorclWindowTextFont.Height�	Font.NameMS Sans Serif
Font.Style OldCreateOrderPositionpoMainFormCenterVisible	OnClose	FormCloseOnCloseQueryFormCloseQueryOnCreate
FormCreate	OnDestroyFormDestroyPixelsPerInch`
TextHeight TShapeShape1Left Top Width#HeightAlignalTopBrush.Color	clBtnFace	Pen.StylepsClear  	TGroupBox	GroupBox1Left Top Width!Height� AlignalCustomAnchorsakLeftakTopakRight CaptionInput parametersTabOrder 
DesignSize!�   TLabelDevices_LblLeftTop!WidthRHeightCaptionDevice selection:  TLabel	Subch_LblLeftTopIWidthHeightCaptionSubchannel reading mode:  TLabelLabel2LeftToppWidthwHeightCaptionError recovery parameter:  TLabelLabel1LeftTop� WidthRHeightCaptionRetry error count:  TLabelLabel3Left� Top� Width+HeightCaptionColumns:  TLabelLabel4Left Top� WidthEHeightCaptionDisplay format:  TButton	RescanBtnLeft�TopWidthQHeightAnchorsakTopakRight CaptionRescanTabOrder OnClickRescanBtnClick  	TComboBox	CD_dev_CBLeftTop0WidthHeightStylecsDropDownListAnchorsakLeftakTopakRight Font.CharsetDEFAULT_CHARSET
Font.ColorclWindowTextFont.Height�	Font.NameMS Sans Serif
Font.Style 
ItemHeight
ParentFontTabOrderOnChangeCD_dev_CBChange  	TComboBoxSubch_CBLeftTopXWidthHeightStylecsDropDownListAnchorsakLeftakTopakRight Font.CharsetDEFAULT_CHARSET
Font.ColorclWindowTextFont.Height�	Font.NameMS Sans Serif
Font.Style 
ItemHeight	ItemIndex
ParentFontTabOrderText1P-W RAW interleaved, 96 bytes/sector (Code: 001b)OnChangeSubch_CBChangeItems.StringsNo subs1P-W RAW interleaved, 96 bytes/sector (Code: 001b)KP-W PACKED de-interleaved, or RAW interleaved, 96 bytes/sector (Code: 100b)7Q or P & Q de-interleaved, 16 bytes/sector (Code: 010b)   	TComboBoxCB_error_recv_paramLeftTop� WidthHeightStylecsDropDownListAnchorsakLeftakTopakRight 
ItemHeight	ItemIndex TabOrderTextNormalOnChangeCB_error_recv_paramChangeItems.StringsNormal$Transfer erraneous block with ECC on%Transfer erraneous block with ECC off   	TCheckBoxCHK_YB_scrambleLeftpTop� Width� HeightCaptionApply YB scramblingColorclInactiveBorderParentColorTabOrderOnClickCHK_YB_scrambleClick  	TCheckBoxCHK_deint_subsLeft Top� Width� HeightCaptionGroup subch bitsChecked	ColorclInactiveBorderParentColorState	cbCheckedTabOrderOnClickCHK_deint_subsClick  TEditTB_retry_countLeftTop� WidthaHeightAutoSizeFont.CharsetDEFAULT_CHARSET
Font.ColorclWindowTextFont.Height�	Font.NameMS Sans Serif
Font.Style 	MaxLength
ParentFontTabOrderOnExitTB_retry_countExit
OnKeyPressTB_retry_countKeyPress  TEditLBA_editLeftTop� WidthaHeightAutoSizeFont.CharsetDEFAULT_CHARSET
Font.ColorclWindowTextFont.Height�	Font.NameMS Sans Serif
Font.Style 	MaxLength
ParentFontTabOrderOnExitLBA_editExit
OnKeyPressLBA_editKeyPress  TButtonPrevSect_BtnLeftpTop� WidthHeightCaption<TabOrderOnClickPrevSect_BtnClick  TButtonNextSect_BtnLeft� Top� WidthHeightCaption>TabOrder	OnClickNextSect_BtnClick  TEditED_line_lenLeft� Top� Width9Height	MaxLengthTabOrder
Text16  	TComboBoxCB_disp_formatLeft Top� WidthaHeightStylecsDropDownList
ItemHeight	ItemIndex TabOrderTextHex + ASCIIOnChangeCB_disp_formatChangeItems.StringsHex + ASCIIHexadecimalBinaryDecimal   	TCheckBoxCB_word_wrapLefthTop� WidthIHeightCaption	Word wrapColorclInactiveBorderParentColorTabOrderOnClickCB_word_wrapClick  TButton	RereadBtnLeft�Top� WidthQHeightAnchorsakTopakRight CaptionRead sectorTabOrderOnClickRereadBtnClick  	TCheckBoxNoErrorMsg_ChkLeft�Top� Width� HeightCaptionNo read error msg boxChecked	ColorclInactiveBorderParentColorParentShowHintShowHint	State	cbCheckedTabOrder   	TGroupBox	GroupBox2Left Top� Width!Height� AlignalCustomAnchorsakLeftakTopakRightakBottom CaptionSector dataTabOrder
DesignSize!�   TPanelPanel1LeftTopWidthHeight� AnchorsakLeftakTopakRightakBottom 
BevelOuterbvNoneTabOrder  	TSplitter	Splitter1Left Top WidthHeightCursorcrVSplitAlignalBottom  TMemo
SectorViewLeft Top WidthHeight AlignalClientConstraints.MinHeightFont.CharsetANSI_CHARSET
Font.ColorclWindowTextFont.Height�	Font.NameCourier New
Font.Style 
ParentFontReadOnly	
ScrollBarsssBothTabOrder WantTabs	WordWrap  TMemo	SubChViewLeft Top$WidthHeight}AlignalBottomConstraints.MinHeightFont.CharsetANSI_CHARSET
Font.ColorclWindowTextFont.Height�	Font.NameCourier New
Font.Style 
ParentFontReadOnly	
ScrollBarsssBothTabOrderWordWrap     