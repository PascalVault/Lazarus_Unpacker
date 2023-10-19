unit PV_Unlib2;

{$mode objfpc}{$H+}

//PV Unpack
//https://github.com/PascalVault
//Licence: MIT
//Last update: 2023-10-15
//Car and Driver .LIB

interface

uses
  Classes, SysUtils, PV_Unpack, Dialogs;

  { TUnPak }

type
  TUnlib2 = class(TUnpack)
  public
    constructor Create(Str: TStream); override;
  end;


implementation

{ TUntar }

constructor TUnlib2.Create(Str: TStream);
type THead = packed record
      Magic: array[0..4] of Char; //"EALIB"
      Count: Word; 
    end;
    TEntry = packed record
      FName: array[0..12] of Char;
      Unk: Byte;
      Offset: Cardinal;
    end;

var Head: THead;
    Entry: TEntry;
    i: Integer;
begin
  FStream := Str;

  try
    FSize  := 1000;
    SetLength(FFiles, FSize);
    FCount := 0;

    FStream.Read(Head, SizeOf(Head));

    for i:=0 to Head.Count-2 do begin

      FStream.Read(Entry, SizeOf(Entry));

      if FCount = FSize then begin
        Inc(FSize, 1000);
        SetLength(FFiles, FSize);
      end;

      FFiles[FCount].Name := Entry.FName;
      FFiles[FCount].Offset := Entry.Offset;
      FFiles[FCount].PackedSize := 0;
      FFiles[FCount].UnpackedSize := 0;
      FFiles[FCount].PackMethod := pmStore;

      Inc(FCount);
    end;

    for i:=0 to Head.Count-2 do begin
      FFiles[i].PackedSize   := FFiles[i+1].Offset - FFiles[i].Offset;
      FFiles[i].UnpackedSize := FFiles[i].PackedSize;
    end;

  except
  end;
end;

end.

