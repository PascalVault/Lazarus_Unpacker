unit PV_Undlt;

{$mode objfpc}{$H+}

//PV Unpack
//https://github.com/PascalVault
//Licence: MIT
//Last update: 2023-10-24
//Stargunner .DLT

interface

uses
  Classes, SysUtils, PV_Unpack, Dialogs;

  { TUnPak }

type

  { TUndlt }

  TUndlt = class(TUnpack)
  private
    function Dec(Str: array of Byte): String;
  public
    constructor Create(Str: TStream); override;
  end;


implementation

{ TUntar }

function TUndlt.Dec(Str: array of Byte): String;
var i: Integer;
    B: Byte;
begin
  Result := chr(Str[0]);

  for i:=1 to 31 do begin
    B := Str[i] xor (Str[i - 1] + i);
    Str[i] := B;

    Result := Result + chr(B);
  end;

  Result := Trim(Result);
end;

constructor TUndlt.Create(Str: TStream);
type THead = packed record
      Magic: array[0..3] of Char; //"DAVE"
      Version: Word;
      Count: Word;
    end;
    TEntry = packed record
      FName: array[0..31] of Byte; //encrypted
      DateTime: Cardinal; //sec since 1980-01-01
      Size: Cardinal;
    end;

var Head: THead;
    Magic: array[0..3] of Char;
    Entry: TEntry;
    Offset: Cardinal;
    i: Integer;
    AFile: TFile;
begin
  inherited Create(Str);

  FStream := Str;

  try
    FStream.Read(Head, SizeOf(Head));

    for i:=0 to Head.Count-1 do begin

      FStream.Read(Entry, SizeOf(Entry));
      FStream.Read(Magic[0], 4);

      //showmessage(magic);

      if Magic = 'PGBP' then Offset := FStream.Position
      else                   Offset := FStream.Position-4;

      AFile.Name := Dec(Entry.FName);
      AFile.Offset := Offset;
      AFile.PackedSize := Entry.Size;
      AFile.UnpackedSize := Entry.Size;
      AFile.PackMethod := pmDLT;
      AFile.ModDate := Unk2DateTime(Entry.DateTime);
      AFile.CRC32 := 0;

      AddFile(AFile);

      FStream.Position := FStream.Position + Entry.Size -4 ;
    end;

  except
  end;
end;

end.

