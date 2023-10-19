unit PV_Unbin;

{$mode objfpc}{$H+}

//PV Unpack
//https://github.com/PascalVault
//Licence: MIT
//Last update: 2023-10-15
//Chasm: The Rift .BIN

interface

uses
  Classes, SysUtils, PV_Unpack, Dialogs;

  { TUnPak }

type
  TUnbin = class(TUnpack)
  public
    constructor Create(Str: TStream); override;
  end;


implementation

{ TUntar }

constructor TUnbin.Create(Str: TStream);
type THead = packed record
      Magic: array[0..3] of Char; //"CSid"
      Count: Word;
    end;
    TEntry = packed record
      FNameLen: Byte;
      FName: array[0..11] of Char;
      Size: Cardinal;
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

    for i:=0 to Head.Count-1 do begin

      FStream.Read(Entry, SizeOf(Entry));

      if FCount = FSize then begin
        Inc(FSize, 1000);
        SetLength(FFiles, FSize);
      end;

      FFiles[FCount].Name := Entry.FName;
      FFiles[FCount].Offset := Entry.Offset;
      FFiles[FCount].PackedSize := Entry.Size;
      FFiles[FCount].UnpackedSize := Entry.Size;
      FFiles[FCount].PackMethod := pmStore;

      Inc(FCount);
    end;
  except
  end;
end;

end.

