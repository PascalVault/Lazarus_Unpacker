unit PV_Unwad2;

{$mode objfpc}{$H+}

//PV Unpack
//https://github.com/PascalVault
//Licence: MIT
//Last update: 2023-09-18
//Quake .WAD

interface

uses
  Classes, SysUtils, PV_Unpack, Dialogs;

  { TUnPak }

type
  TUnwad2 = class(TUnpack)
  public
    constructor Create(Str: TStream); override;
  end;


implementation

{ TUntar }

constructor TUnwad2.Create(Str: TStream);
type THead = packed record
      Magic: array[0..3] of Char;
      TableCount: Cardinal;
      TableOffset: Cardinal;
    end;
    TEntry = packed record
      Offset: Cardinal;
      PackSize: Cardinal;
      UnpackSize: Cardinal;
      Typee: Byte;
      Compression: Byte;
      Dummy: Word;
      FName: array[0..15] of Char;
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

    FStream.Position := Head.TableOffset;

    for i:=0 to Head.TableCount-1 do begin

      FStream.Read(Entry, SizeOf(Entry));

      if FCount = FSize then begin
        Inc(FSize, 1000);
        SetLength(FFiles, FSize);
      end;

      FFiles[FCount].Name := Entry.FName;
      FFiles[FCount].Offset := Entry.Offset;
      FFiles[FCount].PackedSize := Entry.PackSize;
      FFiles[FCount].UnpackedSize := Entry.UnpackSize;

      if Entry.Compression = 0 then FFiles[FCount].PackMethod := pmStore
      else                          FFiles[FCount].PackMethod := pmOther;

      Inc(FCount);
    end;
  except
  end;
end;

end.

