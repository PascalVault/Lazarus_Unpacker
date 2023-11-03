unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, Grids,
  ExtCtrls, ComCtrls, Buttons, PV_Unpack, PV_Unpacker;

type

  { TForm1 }

  TForm1 = class(TForm)
    OpenDialog1: TOpenDialog;
    Panel1: TPanel;
    SaveDialog1: TSaveDialog;
    SG: TStringGrid;
    SpeedButton1: TSpeedButton;
    SpeedButton2: TSpeedButton;
    SpeedButton3: TSpeedButton;
    SpeedButton4: TSpeedButton;
    SpeedButton5: TSpeedButton;
    SpeedButton6: TSpeedButton;
    StatusBar1: TStatusBar;
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormDropFiles(Sender: TObject; const FileNames: array of string);
    procedure SpeedButton1Click(Sender: TObject);
    procedure SpeedButton2Click(Sender: TObject);
    procedure SpeedButton3Click(Sender: TObject);
    procedure SpeedButton4Click(Sender: TObject);
    procedure SpeedButton5Click(Sender: TObject);
    procedure SpeedButton6Click(Sender: TObject);
  public
    OpenedFilename: String;
    Unp: TUnpacker;
    function GetSelected: Integer;
    procedure OpenArchive(FileName: String);
  end;

var
  Form1: TForm1;

implementation

uses test_box, about_box, info_box, preview_box;

{$R *.lfm}

{ TForm1 }

function DateToStr2(D: TDateTime): String;
begin
  if D = 0 then Result := ''
  else Result := DateToStr(D);
end;


procedure TForm1.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
 if Unp <> nil then Unp.Free;
end;

procedure TForm1.FormDropFiles(Sender: TObject; const FileNames: array of string
  );
begin
  OpenArchive(FileNames[0]);
end;

procedure TForm1.SpeedButton1Click(Sender: TObject);
begin
 if not OpenDialog1.Execute then Exit;

 OpenArchive(OpenDialog1.Filename);
end;

procedure TForm1.OpenArchive(FileName: String);
var i: Integer;
begin
  if Unp <> nil then Unp.Free;

  OpenedFilename := FileName;

  Unp := TUnpacker.Create(OpenedFilename);

  if Unp.GetFormat = '' then begin
    ShowMessage('Unsupported');
    Exit;
  end;

  SG.RowCount := 1+Unp.Count;
  SG.Rows[0].CommaText := 'Name,Size,"Unpacked size",Modified,CRC';

  for i:=0 to Unp.Count-1 do begin
    SG.Cells[0, i+1] := Unp.GetName(i);
    SG.Cells[1, i+1] := IntToStr(Unp.GetPackedSize(i));
    SG.Cells[2, i+1] := IntToStr(Unp.GetSize(i));
    SG.Cells[3, i+1] := DateToStr2(Unp.GetDate(i));
    SG.Cells[4, i+1] := IntToHex( Unp.GetCRC(i));
  end;
end;

procedure TForm1.SpeedButton2Click(Sender: TObject);
var Index: Integer;
begin
 Index := GetSelected;
 if Index < 0 then Exit;

 if not Unp.CanUnpack(Index) then begin
    ShowMessage('Unsupported compression');
    Exit;
 end;

 SaveDialog1.Filename := ExtractFileName(Unp.GetName(Index));

 if not SaveDialog1.Execute then Exit;

 Unp.Extract(Index, SaveDialog1.Filename);
end;

procedure TForm1.SpeedButton3Click(Sender: TObject);
var i: Integer;
    Mem: TMemoryStream;
    Res: String;
    Ret: TOpResult;
begin
  Mem := TMemoryStream.Create;
  TestForm.ListBox1.Clear;

  for i:=0 to Unp.Count-1 do begin
    Mem.Clear;

    if not Unp.CanUnpack(i) then Res := 'UNSUPPORTED'
    else begin
      Ret := Unp.Extract(i, Mem);

      case Ret of
        orFail     : Res := 'FAILED';
        orOK       : Res := 'NO PROBLEMS';
        orVerified : Res := 'CRC OK';
      end;
    end;

    TestForm.ListBox1.Items.Add(Unp.GetName(i) + ' - ' + Res);
  end;

  Mem.Free;

  TestForm.Show;
end;

procedure TForm1.SpeedButton4Click(Sender: TObject);
var Str: TMemoryStream;
    Index: Integer;
begin
  Index := GetSelected;
  if Index < 0 then Exit;

  if not Unp.CanUnpack(Index) then begin
    ShowMessage('Unsupported compression');
    Exit;
  end;

  Str := TMemoryStream.Create;

  Unp.Extract(Index, Str);

  Str.Position := 0;
  PreviewForm.Memo1.Lines.LoadFromStream(Str);
  Str.Free;

  PreviewForm.Show;
end;

procedure TForm1.SpeedButton5Click(Sender: TObject);
var SizePacked, Size: Cardinal;
    i: Integer;
begin
 SizePacked := 0;
 Size := 0;

 for i:=0 to Unp.Count-1 do begin
   Inc(SizePacked, Unp.GetPackedSize(i));
   Inc(Size, Unp.GetSize(i));
 end;

 InfoForm.Label2.Caption := ExtractFileName(OpenedFilename);
 InfoForm.Label4.Caption := Unp.GetFormat;
 InfoForm.Label6.Caption := IntToStr(Unp.Count);
 InfoForm.Label8.Caption := IntToStr(SizePacked);
 InfoForm.Label10.Caption := IntToStr(Size);
 InfoForm.Label12.Caption := IntToStr(Round(100*SizePacked/Size)) + '%';

 InfoForm.Show;
end;

procedure TForm1.SpeedButton6Click(Sender: TObject);
begin
  AboutForm.Show;
end;

function TForm1.GetSelected: Integer;
var Index: Integer;
begin
 Index := SG.Selection.Top-1;

 if Index < 0 then Exit;
 if Index > Unp.Count-1 then Exit;

 Result := Index;
end;


end.

