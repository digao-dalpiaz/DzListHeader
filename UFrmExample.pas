unit UFrmExample;

interface

uses Vcl.Forms, System.ImageList, Vcl.ImgList, Vcl.Controls, Vcl.StdCtrls,
  System.Classes, UListHeader, System.Types;

type
  TForm1 = class(TForm)
    LH: TListHeader;
    IL: TImageList;
    L: TListBox;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure LHDrawItem(Control: TWinControl; Index: Integer; Rect: TRect;
      State: TOwnerDrawState);
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

uses System.SysUtils, Vcl.Graphics;

type TPerson = class
  ID: Integer;
  Name, Gender, Sector: String;
  Amount: Double;
  Age: Byte; Birthday: String;
  class function New(aID: Integer; const aName, aGender, aSector: String;
    aAge: Byte; const aBirthday: String;
    aAmount: Double): TPerson;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
    L.Items.AddObject('', TPerson.New(1, 'JOHN', 'MALE', 'Finance', 40, '02/03/1979', 800000) );
    L.Items.AddObject('', TPerson.New(2, 'ALFRED', 'MALE', 'Administration', 55, '01/05/1964', 900000) );
    L.Items.AddObject('', TPerson.New(3, 'SARAH', 'FEMALE', 'Technical', 31, '22/07/1988', 120000) );
    L.Items.AddObject('', TPerson.New(4, 'BRED', 'MALE', 'Technical', 30, '24/12/1989', 100000) );
    L.Items.AddObject('', TPerson.New(5, 'ROBSON', 'MALE', 'Technical', 33, '17/04/1986', 80000) );
    L.Items.AddObject('', TPerson.New(6, 'ARTHUR', 'MALE', 'Technical', 39, '26/04/1980', 70000) );
    L.Items.AddObject('', TPerson.New(7, 'RAY', 'MALE', 'Technical', 25, '14/11/1994', 150000) );
end;

procedure TForm1.FormDestroy(Sender: TObject);
var I: Integer;
begin
    for I := 0 to L.Count-1 do
      L.Items.Objects[I].Free;
end;

procedure TForm1.LHDrawItem(Control: TWinControl; Index: Integer; Rect: TRect;
  State: TOwnerDrawState);
var P: TPerson;
    ImgIdx: Integer;
begin
    P := TPerson(L.Items.Objects[Index]);

    if P.Gender = 'MALE' then ImgIdx := 0 else
    if P.Gender = 'FEMALE' then ImgIdx := 1 else
      ImgIdx := -1;

    if ImgIdx<>-1 then
      IL.Draw(L.Canvas, LH.ColByID(4).GetLeft, Rect.Top+1, ImgIdx);

    LH.DwCol(0, Rect, P.ID);
    LH.DwCol(1, Rect, P.Name);
    LH.DwCol(2, Rect, P.Age);
    LH.DwCol(3, Rect, P.Birthday);
    LH.DwCol(4, Rect, P.Gender, 20);
    LH.DwCol(5, Rect, P.Sector);

    if P.Amount>100000 then
      L.Canvas.Font.Color := clGreen
    else
      L.Canvas.Font.Color := clPurple;

    LH.DwCol(6, Rect, FormatFloat('#,##0.000', P.Amount));
end;

{ TPerson }

class function TPerson.New(aID: Integer; const aName, aGender, aSector: String;
    aAge: Byte; const aBirthday: String;
    aAmount: Double): TPerson;
begin
    Result := TPerson.Create;

    Result.ID := aID;
    Result.Name := aName;
    Result.Gender := aGender;
    Result.Sector := aSector;
    Result.Age := aAge;
    Result.Birthday := aBirthday;
    Result.Amount := aAmount;
end;

end.
