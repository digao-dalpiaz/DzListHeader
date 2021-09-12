{------------------------------------------------------------------------------
TDzListHeader component
Developed by Rodrigo Depine Dalpiaz (digao dalpiaz)
Control to create header columns to a list box

https://github.com/digao-dalpiaz/DzListHeader

Please, read the documentation at GitHub link.
------------------------------------------------------------------------------}

unit DzListHeader;

interface

uses Vcl.Controls, System.Classes, Vcl.StdCtrls, Winapi.Messages, Vcl.Graphics,
  Vcl.ExtCtrls, System.Types;

const
  LH_DEF_HEADERHEIGHT = 20;
  LH_DEF_COLORNORMALCOL = $00804000;
  LH_DEF_COLORHOVERCOL = $00F27900;
  LH_DFF_COLORSHAPE = clRed;
  LH_DEF_COLORLINESEL = $006FEAFB;
  LH_DEF_COLORLINEODD = $00F5F5F5;
  LH_DEF_COLORLINENORMAL = clWindow;
  LH_DEF_TEXTMARGIN = 2;

type
  TDzListHeader = class;
  TDzListHeader_DwCol = class;
  TDzListHeader_DwColsPanel = class;
  TDzListHeaderColsEnum = class;

  TDzListHeaderCol = class(TCollectionItem)
  private
    Comp: TDzListHeader;
    CompDw: TDzListHeader_DwCol; //visual component for column painting

    FName: string; //name used to identify the column, and to find / to load / to save (should not be duplicated)
    FData: Pointer; //pointer to free-use
    FAlignment: TAlignment; //align used in ListBox drawitem painting
    FTextFont: TFont;
    FCustomTextFont: Boolean;
    FCaption: string;
    FCaptionEx: string; //extended caption (optional), to show in customization
    FHint: string;
    FWidth: Integer;
    FMinWidth: Integer;
    FMaxWidth: Integer;
    FSizeable: Boolean;
    FVisible: Boolean;
    FCustomizable: Boolean; //column non-customizable won't be loaded/saved

    function GetNormalizedWidth(W: Integer): Integer; //get width fixed by bounds

    procedure SetName(const Value: string);
    procedure SetCaption(const Value: string);
    procedure SetWidth(const Value: Integer);
    procedure SetMaxWidth(const Value: Integer);
    procedure SetMinWidth(const Value: Integer);
    procedure SetVisible(const Value: Boolean);
    procedure SetHint(const Value: string);
    function GetTextFontStored: Boolean;
    procedure OnTextFontChanged(Sender: TObject);
    procedure SetTextFont(const Value: TFont);
  protected
    function GetDisplayName: string; override;
  public
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;

    property Data: Pointer read FData write FData;

    function GetLeft: Integer;
    function GetRight: Integer;
  published
    property Name: string read FName write SetName;
    property Aligmnent: TAlignment read FAlignment write FAlignment default taLeftJustify;
    property TextFont: TFont read FTextFont write SetTextFont stored GetTextFontStored;
    property CustomTextFont: Boolean read FCustomTextFont write FCustomTextFont default False;
    property Caption: string read FCaption write SetCaption;
    property CaptionEx: string read FCaptionEx write FCaptionEx;
    property Hint: string read FHint write SetHint;
    property Width: Integer read FWidth write SetWidth;
    property MinWidth: Integer read FMinWidth write SetMinWidth default 0;
    property MaxWidth: Integer read FMaxWidth write SetMaxWidth default 0;
    property Sizeable: Boolean read FSizeable write FSizeable default True;
    property Visible: Boolean read FVisible write SetVisible default True;
    property Customizable: Boolean read FCustomizable write FCustomizable default True;
  end;

  TDzListHeaderColumns = class(TCollection)
  private
    Comp: TDzListHeader;
    function GetItem(Index: Integer): TDzListHeaderCol;
  protected
    procedure Update(Item: TCollectionItem); override;
  public
    constructor Create(AOwner: TDzListHeader); reintroduce;
    function GetEnumerator: TDzListHeaderColsEnum;

    property Items[Index: Integer]: TDzListHeaderCol read GetItem; default;

    function FindItemID(ID: Integer): TDzListHeaderCol;
    function FindByName(const aName: string): TDzListHeaderCol;
  end;

  TDzListHeaderColsEnum = class{(TInterfacedObject, IEnumerator<TDzListHeaderCol>)}
  private
    List: TDzListHeaderColumns;
    Index: Integer;
  protected
    function GetCurrent: TDzListHeaderCol; virtual;
  public
    constructor Create(xList: TDzListHeaderColumns);
    function MoveNext: Boolean;
    property Current: TDzListHeaderCol read GetCurrent;
  end;

  {TDzListHeader_DwCol
  This component paints the column on header, and eighter the blank space
  after last column. These objects are inside the TDzListHeader_DwColsPanel.}
  TDzListHeader_DwCol = class(TGraphicControl)
  private
    StartPosX: Integer;
    ResizeReady: Boolean; //indicates when mouse is in the resizing area
    ResizeCol: TDzListHeaderCol; //column that will be resized (can be Col ou ColAnt)
    Resizing: Boolean; //indicates resizing in progress (while mouse pressed)

    Moving: Boolean; //indicated column repositioning in progress (while mouse pressed)
    Moving_DwCol: TDzListHeader_DwCol; //column at position in which actual column will be moved

    MouseMoved: Boolean; //indicates if mouse was moved while Down/Move/Up sequence
    Hover: Boolean; //indicated mouse over column (selected)

    Blank: Boolean; //indicates blank area (does not have column related)
    First: Boolean; //indicates the first visible column

    Head: TDzListHeader_DwColsPanel; //object that contains the visible columns
    Comp: TDzListHeader;
    Col, ColAnt: TDzListHeaderCol; //pointer to this column and previous column

    procedure DoUpdMouseResizing; //set resize flags by actual mouse position
  protected
    procedure Paint; override;
    procedure CMMouseenter(var Message: TMessage); message CM_MOUSEENTER;
    procedure CMMouseleave(var Message: TMessage); message CM_MOUSELEAVE;
    procedure MouseMove(Shift: TShiftState; X: Integer; Y: Integer); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X: Integer;
      Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X: Integer;
      Y: Integer); override;
  public
    constructor Create(AOwner: TDzListHeader_DwColsPanel; xCol: TDzListHeaderCol); reintroduce;
  end;

  {TDzListHeader_DwColsPanel
  Object inside the TDzListHeader_Top and contains all visible columns.
  This component does not stay align, because it should be resized according
  the columns size, to work together with scroll.}
  TDzListHeader_DwColsPanel = class(TCustomControl)
  private
    Comp: TDzListHeader;
    BlankArea: TDzListHeader_DwCol; //object for blank area at the end of columns at right

    procedure RepaintCols; //force invalidate to all visible columns
    procedure Build; //update visible columns

    function FindColAtMousePos: TDzListHeader_DwCol; //get column by actual mouse position
  public
    constructor Create(AOwner: TDzListHeader); reintroduce;
  end;

  {TDzListHeader_Top
  Object aligned to the Top of TDzListHeader, and contains only child component
  TDzListHeader_DwColsPanel, that contains the columns}
  TDzListHeader_Top = class(TCustomControl)
  private
    Comp: TDzListHeader;
  protected
    procedure Resize; override;
  public
    constructor Create(AOwner: TDzListHeader); reintroduce;
  end;

  TEvDzListHeaderColumnDraw = procedure(Sender: TObject; Col: TDzListHeaderCol;
    Canvas: TCanvas; Rect: TRect; Hover: Boolean) of object;
  TEvDzListHeaderColumnOp = procedure(Sender: TObject; Col: TDzListHeaderCol) of object;
  TEvDzListHeaderOnDrawItem = procedure(Control: TWinControl; Index: Integer; Rect: TRect;
    State: TOwnerDrawState) of object; //Vcl.StdCtrls.TDrawItemEvent

  TDzListHeader = class(TCustomControl)
  private
    FAbout: string;

    FAllowResize: Boolean;
    FAllowMoving: Boolean;
    FHeaderHeight: Integer;

    FTitleFont: TFont;

    CTop: TDzListHeader_Top;
    Head: TDzListHeader_DwColsPanel;
    FColumns: TDzListHeaderColumns;
    FListBox: TCustomListBox;
    SB: TScrollBar;

    FColorHoverCol: TColor;
    FColorNormalCol: TColor;
    FColorShape: TColor;
    FColorLineSel: TColor;
    FColorLineOdd: TColor;
    FColorLineNormal: TColor;

    FUseOdd: Boolean;
    FAutoDrawTabbedText: Boolean;

    FLineCenter: Boolean;
    FLineTop: Integer;
    FTextMargin: Integer;

    FEvColumnDraw: TEvDzListHeaderColumnDraw;
    FEvColumnClick, FEvColumnRClick, FEvColumnResize,
    FEvMouseEnterCol, FEvMouseLeaveCol: TEvDzListHeaderColumnOp;

    FEvOnDrawItem: TEvDzListHeaderOnDrawItem;

    Shape: TPanel; //dash to indicate resizing or moving

    function GetStored_Columns: Boolean;
    function GetStored_TitleFont: Boolean;

    procedure OnScroll(Sender: TObject; ScrollCode: TScrollCode; var ScrollPos: Integer);
    procedure OnTitleFontChange(Sender: TObject);

    procedure SetListBox(const Value: TCustomListBox);
    procedure SetHeaderHeight(const Value: Integer);

    procedure SetColorNormalCol(const Value: TColor);
    procedure SetColorHoverCol(const Value: TColor);

    procedure SetTitleFont(const Value: TFont);

    procedure CreateShape(bResizing: Boolean);
    procedure FreeShape;

    procedure UpdListBox;

    procedure ListBoxOnDrawItem(Control: TWinControl; Index: Integer; Rect: TRect;
      State: TOwnerDrawState);

    procedure DoEnsureListBoxAssigned;
    procedure DrawTabbedText(Index: Integer; Rect: TRect);
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure LoadCustom(const A: string);
    function SaveCustom: string;

    function ColByID(ID: Integer): TDzListHeaderCol;
    function ColByName(const aName: string): TDzListHeaderCol;

    procedure DwCol(ID: Integer; Rect: TRect; const Value: Variant; Margin: Integer = 0);

    function AddItem(const Ar: TArray<string>): Integer;
    function GetItemArray(Index: Integer): TArray<string>;
  published
    property About: string read FAbout;

    property Anchors;
    property Align;
    property Enabled;
    property Visible;
    property ShowHint;
    property ParentShowHint;
    property PopupMenu;

    property ListBox: TCustomListBox read FListBox write SetListBox;
    property Columns: TDzListHeaderColumns read FColumns write FColumns stored GetStored_Columns;

    property AllowResize: Boolean read FAllowResize write FAllowResize default True;
    property AllowMoving: Boolean read FAllowMoving write FAllowMoving default True;

    property HeaderHeight: Integer read FHeaderHeight write SetHeaderHeight default LH_DEF_HEADERHEIGHT;

    property ColorHoverCol: TColor read FColorHoverCol write SetColorHoverCol default LH_DEF_COLORHOVERCOL;
    property ColorNormalCol: TColor read FColorNormalCol write SetColorNormalCol default LH_DEF_COLORNORMALCOL;
    property ColorShape: TColor read FColorShape write FColorShape default LH_DFF_COLORSHAPE;
    property ColorLineSel: TColor read FColorLineSel write FColorLineSel default LH_DEF_COLORLINESEL;
    property ColorLineOdd: TColor read FColorLineOdd write FColorLineOdd default LH_DEF_COLORLINEODD;
    property ColorLineNormal: TColor read FColorLineNormal write FColorLineNormal default LH_DEF_COLORLINENORMAL;

    property UseOdd: Boolean read FUseOdd write FUseOdd default False;
    property AutoDrawTabbedText: Boolean read FAutoDrawTabbedText write FAutoDrawTabbedText default False;

    property LineCenter: Boolean read FLineCenter write FLineCenter default True;
    property LineTop: Integer read FLineTop write FLineTop default 0;
    property TextMargin: Integer read FTextMargin write FTextMargin default LH_DEF_TEXTMARGIN;

    property TitleFont: TFont read FTitleFont write SetTitleFont stored GetStored_TitleFont;

    property OnColumnDraw: TEvDzListHeaderColumnDraw read FEvColumnDraw write FEvColumnDraw;
    property OnColumnClick: TEvDzListHeaderColumnOp read FEvColumnClick write FEvColumnClick;
    property OnColumnRClick: TEvDzListHeaderColumnOp read FEvColumnRClick write FEvColumnRClick;
    property OnColumnResize: TEvDzListHeaderColumnOp read FEvColumnResize write FEvColumnResize;
    property OnMouseEnterCol: TEvDzListHeaderColumnOp read FEvMouseEnterCol write FEvMouseEnterCol;
    property OnMouseLeaveCol: TEvDzListHeaderColumnOp read FEvMouseLeaveCol write FEvMouseLeaveCol;

    property OnDrawItem: TEvDzListHeaderOnDrawItem read FEvOnDrawItem write FEvOnDrawItem;
  end;

procedure Register;

implementation

uses System.SysUtils, Winapi.Windows, System.Math, Vcl.Forms,
  System.UITypes, System.StrUtils,
  DzListHeaderCustom;
  
const STR_VERSION = '1.7';

procedure Register;
begin
  RegisterComponents('Digao', [TDzListHeader]);
end;

type TAcListBox = class(TCustomListBox); //to access listbox properties

procedure TDzListHeader.CreateShape(bResizing: Boolean);
begin
  Shape := TPanel.Create(Self);
  Shape.BevelOuter := bvNone;
  Shape.ParentBackground := False;
  Shape.Color := FColorShape;
  Shape.Caption := '';
  if bResizing then
  begin
    Shape.Parent := Self;
    Shape.Width := 1;
    Shape.Top := CTop.Height;
    Shape.Height := Height-Shape.Top-IfThen(SB.Visible, SB.Height);
  end else
  begin
    Shape.Parent := Head;
    Shape.Width := 2;
    Shape.Top := 0;
    Shape.Height := Head.Height;
  end;
end;

procedure TDzListHeader.FreeShape;
begin
  if Assigned(Shape) then
    FreeAndNil(Shape);
end;

{ TDzListHeader }

constructor TDzListHeader.Create(AOwner: TComponent);
begin
  inherited;
  ControlStyle := ControlStyle + [csAcceptsControls]; //accept sub-controls

  FAbout := 'Digao Dalpiaz / Version '+STR_VERSION;

  FAllowResize := True;
  FAllowMoving := True;

  FColorHoverCol := LH_DEF_COLORHOVERCOL;
  FColorNormalCol := LH_DEF_COLORNORMALCOL;
  FColorShape := LH_DFF_COLORSHAPE;
  FColorLineSel := LH_DEF_COLORLINESEL;
  FColorLineOdd := LH_DEF_COLORLINEODD;
  FColorLineNormal := LH_DEF_COLORLINENORMAL;

  FLineCenter := True;
  FTextMargin := LH_DEF_TEXTMARGIN;

  FColumns := TDzListHeaderColumns.Create(Self);

  FTitleFont := TFont.Create;
  FTitleFont.Name := 'Segoe UI';
  FTitleFont.Color := clWhite;
  FTitleFont.OnChange := OnTitleFontChange;

  //--Create main panel align at Top
  CTop := TDzListHeader_Top.Create(Self);
  CTop.Parent := Self;
  CTop.Align := alTop;
  //--

  //--Create columns panel (this object remains inside the Top panel)
  Head := TDzListHeader_DwColsPanel.Create(Self);
  Head.Parent := CTop;
  Head.Left := 0; //initial position
  //--

  SetHeaderHeight(LH_DEF_HEADERHEIGHT); //set property and redefine Head height

  //--Create scroll bar control
  SB := TScrollBar.Create(Self);
  SB.Parent := Self;
  SB.Align := alBottom;
  SB.TabStop := False;
  SB.OnScroll := OnScroll;
  //--
end;

destructor TDzListHeader.Destroy;
begin
  FColumns.Free;
  FTitleFont.Free;

  inherited;
end;

procedure TDzListHeader.DoEnsureListBoxAssigned;
begin
  if not Assigned(FListBox) then
    raise Exception.Create('ListBox not assigned');
end;

function TDzListHeader.AddItem(const Ar: TArray<string>): Integer;
var A, Line: string;
begin
  DoEnsureListBoxAssigned;

  for A in Ar do
    Line := Line + A + #9;

  Result := FListBox.Items.Add(Line);
end;

function TDzListHeader.GetItemArray(Index: Integer): TArray<string>;
begin
  DoEnsureListBoxAssigned;

  Result := FListBox.Items[Index].Split([#9]);
end;

function TDzListHeader.ColByID(ID: Integer): TDzListHeaderCol;
begin
  Result := FColumns.FindItemID(ID);
end;

function TDzListHeader.ColByName(const aName: string): TDzListHeaderCol;
begin
  Result := FColumns.FindByName(aName);
end;

procedure TDzListHeader.ListBoxOnDrawItem(Control: TWinControl; Index: Integer;
  Rect: TRect; State: TOwnerDrawState);
begin
  if TAcListBox(FListBox).Font.Color=clWindowText then
    FListBox.Canvas.Font.Color := clBlack; //fix invert color when selected

  FListBox.Canvas.Brush.Color := FColorLineNormal;
  if FUseOdd then if Odd(Index) then FListBox.Canvas.Brush.Color := FColorLineOdd;
  if odSelected in State then FListBox.Canvas.Brush.Color := FColorLineSel;

  FListBox.Canvas.FillRect(Rect);

  if FAutoDrawTabbedText then
    DrawTabbedText(Index, Rect)
  else
  if Assigned(FEvOnDrawItem) then
    FEvOnDrawItem(Control, Index, Rect, State);

  //fix focus rectangle when using colors
  FListBox.Canvas.Font.Color := clBlack;
  FListBox.Canvas.TextOut(0, 0, '');
end;

procedure TDzListHeader.DrawTabbedText(Index: Integer; Rect: TRect);
var Ar: TArray<string>;
    I: Integer;
begin
  Ar := GetItemArray(Index);

  I := 0;
  while I<FColumns.Count do
  begin
    DwCol(I, Rect, Ar[I]);
    Inc(I);
  end;
end;

procedure TDzListHeader.DwCol(ID: Integer; Rect: TRect; const Value: Variant; Margin: Integer = 0);
var C: TDzListHeaderCol;
    R: TRect;
    A: string;
    X, Y: Integer;
    OriginalFont: TFont;
begin
  DoEnsureListBoxAssigned;
  //

  C := FColumns.FindItemID(ID);
  if not C.FVisible then Exit;

  OriginalFont := nil; //avoid warning
  if C.FCustomTextFont then //column has specific font
  begin
    OriginalFont := TFont.Create;
    OriginalFont.Assign(FListBox.Canvas.Font); //save current font

    FListBox.Canvas.Font.Assign(C.FTextFont); //assign custom font
  end;

  R := System.Types.Rect(C.GetLeft+FTextMargin+Margin, Rect.Top, C.GetRight-FTextMargin, Rect.Bottom);

  A := Value;
  X := 0;
  if C.FAlignment in [taRightJustify, taCenter] then
  begin
    X := R.Width-FListBox.Canvas.TextWidth(A);
    if C.FAlignment=taCenter then X := X div 2;
  end;

  if FLineCenter then
    Y := (TAcListBox(FListBox).ItemHeight-FListBox.Canvas.TextHeight('A')) div 2
  else
    Y := FLineTop;

  FListBox.Canvas.TextRect(R, R.Left+X, R.Top+Y, A); //draw text

  //
  if C.FCustomTextFont then
  begin
    FListBox.Canvas.Font.Assign(OriginalFont); //get back original font
    OriginalFont.Free;
  end;
end;

procedure TDzListHeader.UpdListBox;
begin
  if Assigned(FListBox) then
    FListBox.Invalidate;
end;

procedure TDzListHeader.LoadCustom(const A: string);
var Ar: TArray<string>;
    aInfoCol, aName: string;
    Col: TDzListHeaderCol;
    Vis: Boolean;
    I, X, W: Integer;
begin
  Ar := A.Split(['|']);

  FColumns.BeginUpdate;
  try
    for I := 0 to High(Ar) do
    begin
      try
        aInfoCol := Ar[I];

        Vis := not aInfoCol.StartsWith('~');
        if not Vis then Delete(aInfoCol, 1, 1);

        X := Pos('=', aInfoCol);
        if X=0 then
          raise Exception.Create('Separator not found');

        aName := Copy(aInfoCol, 1, X-1);
        if aName='' then
          raise Exception.Create('Blank name');

        Delete(aInfoCol, 1, X);

        if not TryStrToInt(aInfoCol, W) then
          raise Exception.Create('Invalid value');

        Col := FColumns.FindByName(aName);
        if Col<>nil then //the column may no longer exist in the project
          if Col.FCustomizable then
        begin
          Col.Index := I;

          //using published properties to values processing
          Col.Visible := Vis;
          Col.Width := W;
        end;
      except
        on E: Exception do
          raise Exception.CreateFmt('Error on loading customization of header "%s" at index %d ["%s"]: %s',
            [Name, I, Ar[I], E.Message]);
      end;
    end;
  finally
    FColumns.EndUpdate;
  end;
end;

function TDzListHeader.SaveCustom: string;
var C: TDzListHeaderCol;
    I: Integer;
begin
  Result := '';

  for I := 0 to FColumns.Count-1 do
  begin
    C := FColumns[I];

    if C.FCustomizable then
    begin
      if C.FName='' then //the column must have a name to save
        raise Exception.CreateFmt('Column %d without a name', [I]);

      Result := Result+'|'+Format('%s%s=%d', [IfThen(not C.FVisible, '~'), C.FName, C.FWidth]);
    end;
  end;

  Delete(Result, 1, 1);
end;

function TDzListHeader.GetStored_Columns: Boolean;
begin
  Result := FColumns.Count>0;
end;

function TDzListHeader.GetStored_TitleFont: Boolean;
begin
  Result := not (
      (FTitleFont.Charset = DEFAULT_CHARSET)
  and (FTitleFont.Color = clWhite)
  and (FTitleFont.Name = 'Segoe UI')
  and (FTitleFont.Size = 8)
  and (FTitleFont.Style = [])

  and (FTitleFont.Quality = fqDefault)
  and (FTitleFont.Pitch = fpDefault)
  and (FTitleFont.Orientation = 0)
  );
end;

procedure TDzListHeader.SetListBox(const Value: TCustomListBox);
begin
  if Value <> FListBox then
  begin
    //check for correct parent
    if Value<>nil then
      if Value.Parent<>Self then
        raise Exception.Create('ListBox should be inside ListHeader');

    if FListBox<>nil then //old
    begin
      FListBox.RemoveFreeNotification(Self);

      if not (csDesigning in ComponentState) then
        TAcListBox(FListBox).OnDrawItem := nil;
    end;

    if Value<>nil then //new
    begin
      Value.FreeNotification(Self);

      if not (csDesigning in ComponentState) then
        TAcListBox(Value).OnDrawItem := ListBoxOnDrawItem;

      //automatic definitions to listbox

      TAcListBox(Value).Align := alClient;

      if TAcListBox(Value).Style=lbStandard then
      begin
        TAcListBox(Value).Style := lbOwnerDrawFixed;
        TAcListBox(Value).ItemHeight := 20;
      end;
    end;

    FListBox := Value;
  end;
end;

procedure TDzListHeader.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited;

  if (AComponent=FListBox) and (Operation=opRemove) then
    FListBox := nil;
end;

procedure TDzListHeader.OnScroll(Sender: TObject; ScrollCode: TScrollCode;
  var ScrollPos: Integer);
var Limit: Integer;
begin
  Limit := SB.Max{+1}-SB.PageSize; {+1 because PageSize remains always 1 pixel more?}
  if ScrollPos>Limit then ScrollPos := Limit;

  Head.Left := -ScrollPos;

  UpdListBox;
end;

procedure TDzListHeader.SetHeaderHeight(const Value: Integer);
begin
  FHeaderHeight := Value;
  CTop.Height := Value; //update Top panel height (will run resize event)
end;

procedure TDzListHeader.SetTitleFont(const Value: TFont);
begin
  if FTitleFont<>Value then
  begin
    FTitleFont.Assign(Value);
    //Head.RepaintCols; ???
  end;
end;

procedure TDzListHeader.OnTitleFontChange(Sender: TObject);
begin
  Head.RepaintCols;
end;

procedure TDzListHeader.SetColorHoverCol(const Value: TColor);
begin
  if FColorHoverCol<>Value then
  begin
    FColorHoverCol := Value;

    Head.RepaintCols;
  end;
end;

procedure TDzListHeader.SetColorNormalCol(const Value: TColor);
begin
  if FColorNormalCol<>Value then
  begin
    FColorNormalCol := Value;

    Head.RepaintCols;
  end;
end;

{ TDzListHeaderColumns }

constructor TDzListHeaderColumns.Create(AOwner: TDzListHeader);
begin
  inherited Create(TDzListHeaderCol);
  Comp := AOwner;
end;

function TDzListHeaderColumns.FindByName(const aName: string): TDzListHeaderCol;
var Col: TDzListHeaderCol;
begin
  Result := nil;

  if aName='' then
    raise Exception.Create('Name not specified to find');

  for Col in Self do
    if SameText(Col.FName, aName) then
    begin
      Result := Col;
      Break;
    end;
end;

function TDzListHeaderColumns.FindItemID(ID: Integer): TDzListHeaderCol;
begin
  Result := TDzListHeaderCol( inherited FindItemID(ID) );
end;

function TDzListHeaderColumns.GetEnumerator: TDzListHeaderColsEnum;
begin
  Result := TDzListHeaderColsEnum.Create(Self);
end;

function TDzListHeaderColumns.GetItem(Index: Integer): TDzListHeaderCol;
begin
  Result := TDzListHeaderCol( inherited GetItem(Index) );
end;

procedure TDzListHeaderColumns.Update(Item: TCollectionItem);
begin
  inherited;
  Comp.Head.Build; //on change collection properties, needs rebuild columns
end;

{ TDzListHeaderColsEnum }

constructor TDzListHeaderColsEnum.Create(xList: TDzListHeaderColumns);
begin
  List := xList;
  Index := -1;
end;

function TDzListHeaderColsEnum.GetCurrent: TDzListHeaderCol;
begin
  Result := List[Index];
end;

function TDzListHeaderColsEnum.MoveNext: Boolean;
begin
  Result := Index < List.Count-1;
  if Result then Inc(Index);
end;

{ TDzListHeaderCol }

constructor TDzListHeaderCol.Create(Collection: TCollection);
begin
  inherited;
  Comp := TDzListHeaderColumns(Collection).Comp;

  FTextFont := TFont.Create;
  FTextFont.OnChange := OnTextFontChanged;

  FAlignment := taLeftJustify;

  FSizeable := True;
  FVisible := True;
  FCustomizable := True;

  FWidth := 100; //initial width (not default!)
end;

destructor TDzListHeaderCol.Destroy;
begin
  if Assigned(CompDw) then CompDw.Free;
  FTextFont.Free;

  inherited;
end;

function TDzListHeaderCol.GetDisplayName: string;
begin
  Result := FName;
  if FName='' then Result := inherited GetDisplayName;
end;

procedure TDzListHeaderCol.SetName(const Value: string);
begin
  if Value.IndexOfAny(['|','~','='])<>-1 then
    raise Exception.Create('Character not allowed'); //because coding used in load/save customization string

  FName := Trim(Value);
end;

procedure TDzListHeaderCol.SetCaption(const Value: string);
begin
  if FCaption<>Value then
  begin
    FCaption := Value;

    if Assigned(CompDw) then
      CompDw.Invalidate; //repaint this column visual object
  end;
end;

procedure TDzListHeaderCol.SetTextFont(const Value: TFont);
begin
  FTextFont.Assign(Value);
end;

function TDzListHeaderCol.GetTextFontStored: Boolean;
begin
  Result := FCustomTextFont; //textfont property will be stored if is custom
end;

procedure TDzListHeaderCol.OnTextFontChanged(Sender: TObject);
begin
  FCustomTextFont := True;
end;

procedure TDzListHeaderCol.SetHint(const Value: string);
begin
  if FHint<>Value then
  begin
    FHint := Value;

    if Assigned(CompDw) then
      CompDw.Hint := Value; {the hint is set in Build eigther}
  end;
end;

procedure TDzListHeaderCol.SetMaxWidth(const Value: Integer);
begin
  if FMaxWidth<>Value then
  begin
    FMaxWidth := Value;
    if Value<FWidth then SetWidth(Value);
  end;
end;

procedure TDzListHeaderCol.SetMinWidth(const Value: Integer);
begin
  if FMinWidth<>Value then
  begin
    FMinWidth := Value;
    if Value>FWidth then SetWidth(Value);
  end;
end;

function TDzListHeaderCol.GetNormalizedWidth(W: Integer): Integer;
begin
  if FMinWidth>0 then
    if W<FMinWidth then W := FMinWidth;

  if FMaxWidth>0 then
    if W>FMaxWidth then W := FMaxWidth;

  if W<10 then W := 10;

  Result := W;
end;

procedure TDzListHeaderCol.SetWidth(const Value: Integer);
var W: Integer;
begin
  W := GetNormalizedWidth(Value);

  if FWidth<>W then
  begin
    FWidth := W;
    Changed(False);
  end;
end;

procedure TDzListHeaderCol.SetVisible(const Value: Boolean);
begin
  if FVisible<>Value then
  begin
    FVisible := Value;
    Changed(False);
  end;
end;

function TDzListHeaderCol.GetLeft: Integer;
begin
  if not FVisible then
    raise Exception.Create('Column not visible');

  Result := Comp.Head.Left+CompDw.Left;
end;

function TDzListHeaderCol.GetRight: Integer;
begin
  //* The check for visible column already ocurrs on GetLeft

  Result := GetLeft+CompDw.Width;
end;

{ TListHeader_DwPanel }

constructor TDzListHeader_DwColsPanel.Create(AOwner: TDzListHeader);
begin
  inherited Create(AOwner);
  Comp := AOwner;

  BlankArea := TDzListHeader_DwCol.Create(Self, nil);
  BlankArea.Blank := True;
end;

function TDzListHeader_DwColsPanel.FindColAtMousePos: TDzListHeader_DwCol;
var X, iMax: Integer;
begin
  //get visual column by actual mouse position (always return a column)

  X := ScreenToClient(Mouse.CursorPos).X;
  if X<0 then X := 0 else
  begin
    iMax := Width-BlankArea.Width-1; //-1 bacuse the full width already outside objects area
    if X>iMax then X := iMax;
  end;

  Result := TDzListHeader_DwCol( ControlAtPos(Point(X, 0), False) );
end;

procedure TDzListHeader_DwColsPanel.Build;
var Col, ColAnt: TDzListHeaderCol;
    X, W, Wtot: Integer;
    Count: Integer;
    ItFits: Boolean;
const SOBRINHA = 20; //safety space of listbox vertical scroll bar
begin
  if csLoading in Comp.ComponentState then Exit;

  Count := 0;
  ColAnt := nil;
  X := 0;
  for Col in Comp.FColumns do
  begin
    if Col.FVisible then
    begin
      Inc(Count);

      if not Assigned(Col.CompDw) then
        Col.CompDw := TDzListHeader_DwCol.Create(Self, Col);

      Col.CompDw.Hint := Col.FHint; //update hint

      Col.CompDw.First := Count=1;
      Col.CompDw.ColAnt := ColAnt;

      W := Col.FWidth;
      Col.CompDw.SetBounds(X, 0, W, Height);
      Inc(X, W);

      ColAnt := Col;
    end else
    begin
      if Assigned(Col.CompDw) then
        FreeAndNil(Col.CompDw);
    end;
  end;

  Wtot := X+1+SOBRINHA; //get full size including final dash
  ItFits := Wtot<=Comp.CTop.Width;
  if ItFits then //columns and waste fits on total width
    Width := Comp.CTop.Width //set Head width equals full width
  else
    Width := Wtot;

  //--Set blank area at right corner
  BlankArea.SetBounds(X, 0, Width-X, Height);
  BlankArea.ColAnt := ColAnt;
  //--

  X := X+BlankArea.Width; //calc all columns size including blank area
  if X+Left<Comp.CTop.Width then
  begin
    //is decreasing column, then should move all objects to ensure not left blank space
    Left := Comp.CTop.Width-X;
  end;

  Comp.SB.Visible := not ItFits;
  if Comp.SB.Visible then
  begin
    Comp.SB.PageSize := 0; //avoid error
    Comp.SB.Max := Width{-1}; //-1 else scroll always jump 1 pixel more
    Comp.SB.PageSize := Comp.CTop.Width;

    Comp.SB.Position := -Left; //update scroll bar position
  end else
  begin
    Comp.SB.PageSize := 0;
    Comp.SB.Max := 0;
    Comp.SB.Position := 0;
  end;

  Comp.UpdListBox;
end;

procedure TDzListHeader_DwColsPanel.RepaintCols;
var I: Integer;
begin
  //if csLoading in Comp.ComponentState then Exit;

  //Repaint all columns
  for I := 0 to ControlCount-1 do
    if Controls[I] is TDzListHeader_DwCol then //should all be (lol!)
      Controls[I].Invalidate;
end;

{ TDzListHeader_DwCol }

constructor TDzListHeader_DwCol.Create(AOwner: TDzListHeader_DwColsPanel; xCol: TDzListHeaderCol);
begin
  inherited Create(AOwner);
  Head := AOwner;
  Comp := AOwner.Comp;
  Col := xCol;

  Parent := AOwner;

  ShowHint := True;
end;

procedure TDzListHeader_DwCol.CMMouseenter(var Message: TMessage);
begin
  DoUpdMouseResizing;

  if not Blank then
  begin
    Hover := True;
    Invalidate;

    if Assigned(Comp.FEvMouseEnterCol) then
      Comp.FEvMouseEnterCol(Comp, Col);
  end;
end;

procedure TDzListHeader_DwCol.CMMouseleave(var Message: TMessage);
begin
  DoUpdMouseResizing;

  if not Blank then
  begin
    Hover := False;
    Invalidate;

    if Assigned(Comp.FEvMouseLeaveCol) then
      Comp.FEvMouseLeaveCol(Comp, Col);
  end;
end;

procedure TDzListHeader_DwCol.DoUpdMouseResizing;
var P: TPoint;
    OK: Boolean;
    tmpCol: TDzListHeaderCol;
begin
  //if Resizing then Exit;

  P := CalcCursorPos; //get mouse actual position relative to this control

  OK := False;
  tmpCol := nil;

  if Comp.FAllowResize then
    if Comp.FColumns.Count>0 then //if no columns, there be only then blank area, which does not have ColAnt
      if PtInRect(Comp.CTop.ClientRect, Comp.CTop.CalcCursorPos) then //when mouseup outside the area
      //if PtInRect(ClientRect, P) then //mouse is inside this control
        if ((not First) and (P.X<4)) or ((not Blank) and (P.X>Width-4)) then
  begin
    if P.X<4 then
      tmpCol := ColAnt
    else
      tmpCol := Col;

    if tmpCol.FSizeable then
      OK := True;
  end;

  if OK then
  begin
    Screen.Cursor := crHSplit;
    ResizeReady := True;
    ResizeCol := tmpCol;
  end else
  begin
    Screen.Cursor := crDefault;
    ResizeReady := False;
    ResizeCol := nil;
  end;
end;

procedure TDzListHeader_DwCol.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  inherited;

  if Button=mbLeft then
  begin
    StartPosX := X; //save initial position to resize or move
    MouseMoved := False; //reset moved flag

    if ResizeReady then
    begin
      //resizing
      Comp.CreateShape(True);
      Comp.Shape.Left := Head.Left+ResizeCol.CompDw.Left+ResizeCol.CompDw.Width;

      Resizing := True;
    end;
  end;
end;

procedure TDzListHeader_DwCol.MouseMove(Shift: TShiftState; X, Y: Integer);
var I: Integer;
begin
  inherited;

  if ssLeft in Shift then //left mouse pressed
  begin
    MouseMoved := True;

    if Resizing then
    begin
      I := ResizeCol.GetNormalizedWidth(ResizeCol.CompDw.Width+X-StartPosX);
      Comp.Shape.Left := Comp.Head.Left+ResizeCol.CompDw.Left+I;
    end else
    if Comp.FAllowMoving and not Blank then
    begin
      if not Moving then
      begin
        Moving := True;
        Comp.CreateShape(False);
      end;
      Moving_DwCol := Head.FindColAtMousePos;
      I := Moving_DwCol.Left;
      if X>StartPosX then //moving ahead
        Inc(I, Moving_DwCol.Width);
      Comp.Shape.Left := I;
    end;

  end else
    DoUpdMouseResizing;
end;

procedure TDzListHeader_DwCol.MouseUp(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
begin
  inherited;

  if Resizing then
  begin
    Resizing := False;
    Comp.FreeShape;

    ResizeCol.SetWidth(ResizeCol.CompDw.Width+X-StartPosX);

    Comp.UpdListBox;

    if Assigned(Comp.FEvColumnResize) then
      Comp.FEvColumnResize(Comp, ResizeCol);
  end else
  if Moving then
  begin
    Moving := False;
    Comp.FreeShape;

    Col.Index := Moving_DwCol.Col.Index;

    Comp.UpdListBox;
  end else
  if Button=mbRight then DoListHeaderCustomizeDlg(Comp) else

  if (not MouseMoved) and (not Blank) then
  case Button of
    mbLeft:
      if Assigned(Comp.FEvColumnClick) then
        Comp.FEvColumnClick(Comp, Col);

    mbRight:
      if Assigned(Comp.FEvColumnRClick) then
        Comp.FEvColumnRClick(Comp, Col);
  end;
end;

procedure TDzListHeader_DwCol.Paint;
var B: Vcl.Graphics.TBitmap;
    R: TRect;
    Fmt: Cardinal;
    H, altTxt: Integer;
    A: string;
    C: TColor;
begin
  inherited;

  B := Vcl.Graphics.TBitmap.Create;
  try
    B.SetSize(Width, Height);
    //

    if Hover then
      C := Comp.FColorHoverCol
    else
      C := Comp.FColorNormalCol;

    B.Canvas.Brush.Color := C;
    B.Canvas.Pen.Color := clBtnShadow;
    B.Canvas.Rectangle(0, 0, Width+IfThen(not Blank, 1), Height+1);

    if not Blank then
    begin
      B.Canvas.Font.Assign(Comp.FTitleFont);

      A := Col.FCaption;
      Fmt := DT_NOPREFIX;
      H := DrawText(B.Canvas.Handle, A, -1, R, DT_CALCRECT or Fmt); //calc text height
      altTxt := (Height+1-H) div 2; if altTxt<1 then altTxt := 1;
      R := Rect(2, altTxt, Width, Height);
      DrawText(B.Canvas.Handle, A, -1, R, Fmt);

      if Assigned(Comp.FEvColumnDraw) then
        Comp.FEvColumnDraw(Comp, Col, B.Canvas, ClientRect, Hover)
    end;

    //
    Canvas.Draw(0, 0, B);
  finally
    B.Free;
  end;
end;

{ TDzListHeader_Top }

constructor TDzListHeader_Top.Create(AOwner: TDzListHeader);
begin
  inherited Create(AOwner);
  Comp := AOwner;
end;

procedure TDzListHeader_Top.Resize;
begin
  inherited;
  Comp.Head.Height := Height;
  Comp.Head.Build; //rebuild columns (Width or Height may be changed)
end;

end.
