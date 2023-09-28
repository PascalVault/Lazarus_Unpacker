unit PV_Unpck;

{$mode objfpc}{$H+}

//PV Unpack
//https://github.com/PascalVault
//Licence: MIT
//Last update: 2023-09-18
//In the Groove .PCK

interface

uses
  Classes, SysUtils, PV_Unpack, Dialogs;

  { TUnPak }

type
  TUnpck = class(TUnpack)
  public
    constructor Create(Str: TStream); override;
  end;

implementation

{ TUntar }

constructor TUnpck.Create(Str: TStream);
type THead = packed record
       Magic: array[0..3] of Char; //PCKF
       Comment: array[0..127] of Char;
       TableCount: Cardinal;
    end;
    TEntry = packed record
       UnpackedSize: Cardinal;
       PackedSize: Cardinal;
       Offset: Cardinal;
       FNameLen: Cardinal;
       Compression: Cardinal; //0=store
    end;

var Head: THead;
    Entry: TEntry;
    i: Integer;
    FName: String;
begin
  FStream := Str;

  try
    FSize  := 1000;
    SetLength(FFiles, FSize);
    FCount := 0;

    FStream.Read(Head, SizeOf(Head));

    for i:=0 to Head.TableCount-1 do begin

      FStream.Read(Entry, SizeOf(Entry));

      SetLength(FName, Entry.FNameLen);
      FStream.Read(FName[1], Entry.FNameLen);

      if FCount = FSize then begin
        Inc(FSize, 1000);
        SetLength(FFiles, FSize);
      end;

      FFiles[FCount].Name := FName;
      FFiles[FCount].Offset := Entry.Offset;
      FFiles[FCount].PackedSize := Entry.PackedSize;
      FFiles[FCount].UnpackedSize := Entry.UnpackedSize;

      if Entry.Compression = 0 then FFiles[FCount].PackMethod := pmStore
      else                          FFiles[FCount].PackMethod := pmDeflate; //was described as ZLIB but actually just deflate

      Inc(FCount);
    end;
  except
  end;
end;

end.

