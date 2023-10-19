unit PV_Ungx;

{$mode objfpc}{$H+}

//PV Unpack
//https://github.com/PascalVault
//Licence: MIT
//Last update: 2023-10-15
//Word Rescue .2

interface

uses
  Classes, SysUtils, PV_Unpack, Dialogs;

  { TUnPak }

type
  TUngx = class(TUnpack)
  public
    constructor Create(Str: TStream); override;
  end;


implementation

{ TUntar }

constructor TUngx.Create(Str: TStream);
type THead = packed record
      Magic: Word; //$01 $CA
      Copyright: array[0..49] of Char;
      Version: Word;
      Caption: array[0..39] of Char;
      Count: Word;
      Reserved: array[0..31] of Char;
    end;
    TEntry = packed record
      Method: Byte;
      FName: array[0..12] of Char;
      Offset: Cardinal;
      Size: Cardinal;
      FileDate: Word;
      FileTime: Word;
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

