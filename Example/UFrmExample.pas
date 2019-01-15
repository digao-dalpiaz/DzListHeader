unit UFrmExample;

interface

uses Vcl.Forms, Vcl.Controls, Vcl.StdCtrls, System.Classes, UListHeader,
  System.Types, System.ImageList, Vcl.ImgList;

type
  TForm1 = class(TForm)
    LH: TListHeader;
    L: TListBox;
    IL: TImageList;
    procedure LDrawItem(Control: TWinControl; Index: Integer; Rect: TRect;
      State: TOwnerDrawState);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

uses System.SysUtils;

type TPerson = class
  ID: Integer;
  Name, Gender, Sector: String;
  Amount: Double;
  class function New(aID: Integer; const aName, aGender, aSector: String;
    aAmount: Double): TPerson;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
    L.Items.AddObject('', TPerson.New(1, 'JOHN', 'MALE', 'Finance', 800000) );
    L.Items.AddObject('', TPerson.New(2, 'ALFRED', 'MALE', 'Administration', 900000) );
    L.Items.AddObject('', TPerson.New(3, 'SARAH', 'FEMALE', 'Technical', 120000) );
    L.Items.AddObject('', TPerson.New(4, 'BRED', 'MALE', 'Technical', 100000) );
    L.Items.AddObject('', TPerson.New(5, 'ROBSON', 'MALE', 'Technical', 80000) );
    L.Items.AddObject('', TPerson.New(6, 'ARTHUR', 'MALE', 'Technical', 70000) );
    L.Items.AddObject('', TPerson.New(7, 'RAY', 'MALE', 'Technical', 150000) );
end;

procedure TForm1.FormDestroy(Sender: TObject);
var I: Integer;
begin
    for I := 0 to L.Count-1 do
      L.Items.Objects[I].Free;
end;

procedure TForm1.LDrawItem(Control: TWinControl; Index: Integer; Rect: TRect;
  State: TOwnerDrawState);
var P: TPerson;
    ImgIdx: Integer;
begin
    LH.InitDrawItem(Index, Rect, State);

    P := TPerson(L.Items.Objects[Index]);

    if P.Gender = 'MALE' then ImgIdx := 0 else
    if P.Gender = 'FEMALE' then ImgIdx := 1 else
      ImgIdx := -1;

    if ImgIdx<>-1 then
      IL.Draw(L.Canvas, LH.ColByID(2).GetLeft, Rect.Top+1, ImgIdx);

    LH.DwCol(0, Rect, P.ID);
    LH.DwCol(1, Rect, P.Name);
    LH.DwCol(2, Rect, P.Gender, 20);
    LH.DwCol(3, Rect, P.Sector);
    LH.DwCol(4, Rect, FormatFloat('#,##0.000', P.Amount));
end;

{ TPerson }

class function TPerson.New(aID: Integer; const aName, aGender, aSector: String;
    aAmount: Double): TPerson;
begin
    Result := TPerson.Create;

    Result.ID := aID;
    Result.Name := aName;
    Result.Gender := aGender;
    Result.Sector := aSector;
    Result.Amount := aAmount;
end;

end.
