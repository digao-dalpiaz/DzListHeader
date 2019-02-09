unit UFrmExample;

interface

uses Vcl.Forms, System.ImageList, Vcl.ImgList, Vcl.Controls, Vcl.StdCtrls,
  System.Classes, System.Types, Vcl.Graphics, DzListHeader;

type
  TForm1 = class(TForm)
    LH: TDzListHeader;
    IL: TImageList;
    L: TListBox;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure LHDrawItem(Control: TWinControl; Index: Integer; Rect: TRect;
      State: TOwnerDrawState);
    procedure LHColumnDraw(Sender: TObject; Col: TDzListHeaderCol;
      Canvas: TCanvas; Rect: TRect; Hover: Boolean);
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

uses System.SysUtils, System.IniFiles;

type TPerson = class
  ID: Integer;
  Name, Gender, Sector: String;
  Amount: Double;
  Age: Byte; Birthday: String;
  class function New(aID: Integer; const aName, aGender, aSector: String;
    aAge: Byte; const aBirthday: String;
    aAmount: Double): TPerson;
end;

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

//

function GetIniFile: String;
begin
  Result := ExtractFilePath(Application.ExeName)+'Custom.ini';
end;

procedure TForm1.FormCreate(Sender: TObject);
var Ini: TIniFile;
begin
  //Load customization from ini file
  Ini := TIniFile.Create(GetIniFile);
  try
    LH.LoadCustom( Ini.ReadString('ListHeader', 'Columns', '') );
  finally
    Ini.Free;
  end;

  //Create sample items objects
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
    Ini: TIniFile;
begin
  for I := 0 to L.Count-1 do
    L.Items.Objects[I].Free;

  //Save customization to ini file
  Ini := TIniFile.Create(GetIniFile);
  try
    Ini.WriteString('ListHeader', 'Columns', LH.SaveCustom);
  finally
    Ini.Free;
  end;
end;

procedure TForm1.LHColumnDraw(Sender: TObject; Col: TDzListHeaderCol;
  Canvas: TCanvas; Rect: TRect; Hover: Boolean);
begin
  if Col.ID=3 then //birthday column
    IL.Draw(Canvas, Rect.Right-20, Rect.Top+2, 2);
end;

procedure TForm1.LHDrawItem(Control: TWinControl; Index: Integer; Rect: TRect;
  State: TOwnerDrawState);
var P: TPerson;
    ImgIdx: Integer;
    ColGender: TDzListHeaderCol;
begin
  P := TPerson(L.Items.Objects[Index]);

  ColGender := LH.ColByID(4); //get "Gender" column object
  if ColGender.Visible then
  begin
    if P.Gender = 'MALE' then ImgIdx := 0 else
    if P.Gender = 'FEMALE' then ImgIdx := 1 else
      ImgIdx := -1;

    if ImgIdx<>-1 then
      IL.Draw(L.Canvas, ColGender.GetLeft, Rect.Top+1, ImgIdx);
  end;

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

end.
