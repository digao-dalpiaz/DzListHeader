unit UFrmExample;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, UListHeader;

type
  TForm1 = class(TForm)
    LH: TListHeader;
    L: TListBox;
    procedure LDrawItem(Control: TWinControl; Index: Integer; Rect: TRect;
      State: TOwnerDrawState);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

type TPerson = class
  ID: Integer;
  Name, Gender: String;
  class function New(aID: Integer; const aName, aGender: String): TPerson;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
    L.Items.AddObject('', TPerson.New(1, 'JOHN', 'MALE') );
    L.Items.AddObject('', TPerson.New(2, 'ALFRED', 'MALE') );
    L.Items.AddObject('', TPerson.New(3, 'SARAH', 'FEMALE') );
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
begin
    LH.InitDrawItem(Index, Rect, State);

    P := TPerson(L.Items.Objects[Index]);

    LH.DwCol(0, Rect, P.ID);
    LH.DwCol(1, Rect, P.Name);
    LH.DwCol(2, Rect, P.Gender);
end;

{ TPerson }

class function TPerson.New(aID: Integer; const aName, aGender: String): TPerson;
begin
    Result := TPerson.Create;

    Result.ID := aID;
    Result.Name := aName;
    Result.Gender := aGender;
end;

end.
