unit UFrmListHeaderCustom;

interface

uses Vcl.Forms, Vcl.StdCtrls, Vcl.Controls, Vcl.ExtCtrls, Vcl.CheckLst,
  System.Classes, Vcl.Buttons,
  //
  ListHeader, System.Types;

type
  TFrmListHeaderCustom = class(TForm)
    BtnAll: TSpeedButton;
    BtnNone: TSpeedButton;
    BtnSwap: TSpeedButton;
    BtnDefOrder: TSpeedButton;
    L: TCheckListBox;
    BoxBtns: TPanel;
    BtnOK: TButton;
    BtnCancel: TButton;
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure LDragDrop(Sender, Source: TObject; X, Y: Integer);
    procedure LDragOver(Sender, Source: TObject; X, Y: Integer;
      State: TDragState; var Accept: Boolean);
    procedure LDrawItem(Control: TWinControl; Index: Integer; Rect: TRect;
      State: TOwnerDrawState);
    procedure BtnAllClick(Sender: TObject);
    procedure BtnNoneClick(Sender: TObject);
    procedure BtnSwapClick(Sender: TObject);
    procedure BtnDefOrderClick(Sender: TObject);
    procedure BtnOKClick(Sender: TObject);
  private
    LH: TListHeader;

    DropIndex: Integer;

    procedure Load;
    procedure AddCol(C: TListHeaderCol);
  end;

var
  FrmListHeaderCustom: TFrmListHeaderCustom;

procedure DoListHeaderCustomizeDlg(LH: TListHeader);

implementation

{$R *.dfm}

procedure DoListHeaderCustomizeDlg;
begin
    FrmListHeaderCustom := TFrmListHeaderCustom.Create(Application);
    FrmListHeaderCustom.LH := LH;
    FrmListHeaderCustom.ShowModal;
    FrmListHeaderCustom.Free;
end;

//

procedure TFrmListHeaderCustom.FormCreate(Sender: TObject);
begin
    L.Anchors := [akLeft,akRight,akTop,akBottom];
    BoxBtns.Anchors := [akBottom];

    DropIndex := -1;
end;

procedure TFrmListHeaderCustom.FormShow(Sender: TObject);
begin
    Load;
end;

procedure TFrmListHeaderCustom.AddCol(C: TListHeaderCol);
var A: String;
    I: Integer;
begin
    A := C.CaptionEx;
    if A='' then A := C.Caption;

    I := L.Items.AddObject(A, C);
    L.Checked[I] := C.Visible;
end;

procedure TFrmListHeaderCustom.Load;
var C: TListHeaderCol;
begin
    for C in LH.Columns do
    begin
      if C.Customizable then
        AddCol(C);
    end;
end;

procedure TFrmListHeaderCustom.BtnOKClick(Sender: TObject);
var I: Integer;
    C: TListHeaderCol;
begin
    LH.Columns.BeginUpdate;
    try
      for I := 0 to L.Count-1 do
      begin
        C := TListHeaderCol(L.Items.Objects[I]);

        C.Index := I;
        C.Visible := L.Checked[I];
      end;
    finally
      LH.Columns.EndUpdate;
    end;

    ModalResult := mrOk;
end;

procedure TFrmListHeaderCustom.BtnDefOrderClick(Sender: TObject);
var I: Integer;
    C: TListHeaderCol;
begin
    L.Clear;
    for I := 0 to LH.Columns.Count-1 do
    begin
      C := LH.Columns.FindItemID(I);
      if C.Customizable then
        AddCol(C);
    end;
end;

procedure TFrmListHeaderCustom.LDragOver(Sender, Source: TObject; X, Y: Integer;
  State: TDragState; var Accept: Boolean);
var OldIndex: Integer;
begin
    if L.ItemIndex=-1 then
    begin
      Accept := False;
      Exit;
    end;

    OldIndex := DropIndex;

    DropIndex := L.ItemAtPos(Point(X,Y), False);
    if DropIndex=L.Count then Dec(DropIndex);

    if OldIndex<>DropIndex then
      L.Invalidate;
end;

procedure TFrmListHeaderCustom.LDrawItem(Control: TWinControl; Index: Integer;
  Rect: TRect; State: TOwnerDrawState);
begin
    L.Canvas.FillRect(Rect);

    if (Index=DropIndex) and (Index<>L.ItemIndex) then
    begin
      if L.ItemIndex>Index then
        L.Canvas.Rectangle(Rect.Left, Rect.Top, Rect.Right, Rect.Top+1)
      else
        L.Canvas.Rectangle(Rect.Left, Rect.Bottom, Rect.Right, Rect.Bottom-1)
    end;

    L.Canvas.TextOut(Rect.Left+2, Rect.Top+2, L.Items[Index]);
end;

procedure TFrmListHeaderCustom.LDragDrop(Sender, Source: TObject; X,
  Y: Integer);
begin
    if DropIndex<>L.ItemIndex then
    begin
      L.Items.Move(L.ItemIndex, DropIndex);
      L.ItemIndex := DropIndex;
      //L.Invalidate;
    end;

    DropIndex := -1;
end;

procedure TFrmListHeaderCustom.BtnAllClick(Sender: TObject);
begin
    L.CheckAll(cbChecked);
end;

procedure TFrmListHeaderCustom.BtnNoneClick(Sender: TObject);
begin
    L.CheckAll(cbUnchecked);
end;

procedure TFrmListHeaderCustom.BtnSwapClick(Sender: TObject);
var I: Integer;
begin
    for I := 0 to L.Count-1 do
      L.Checked[I] := not L.Checked[I];
end;

end.
