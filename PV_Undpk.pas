unit PV_Undpk;

{$mode objfpc}{$H+}

//PV Unpack
//https://github.com/PascalVault
//Licence: MIT
//Last update: 2023-09-18
//Bunny Pro. Das2 .DPK

interface

uses
  Classes, SysUtils, PV_Unpack, Dialogs;

  { TUnPak }

type
  TUndpk = class(TUnpack)
  public
    constructor Create(Str: TStream); override;
  end;


implementation

{ TUntar }

constructor TUndpk.Create(Str: TStream);
type THead = packed record
       Magic: array[0..1] of Char; //PA
       TableCount: Word;
       TotalSize: Cardinal;
    end;
    TEntry = packed record
       FName: array[0..15] of Char; //null pad
       Size: Cardinal;
    end;

var Head: THead;
    Entry: TEntry;
    i: Integer;
    Offset: Cardinal;
begin
  FStream := Str;

  try
    FSize  := 1000;
    SetLength(FFiles, FSize);
    FCount := 0;

    FStream.Read(Head, SizeOf(Head));

    Offset := SizeOf(Head) + Head.TableCount * SizeOf(Entry);

    for i:=0 to Head.TableCount-1 do begin

      FStream.Read(Entry, SizeOf(Entry));

      if FCount = FSize then begin
        Inc(FSize, 1000);
        SetLength(FFiles, FSize);
      end;

      FFiles[FCount].Name := Entry.FName;
      FFiles[FCount].Offset := Offset;
      FFiles[FCount].PackedSize := Entry.Size;
      FFiles[FCount].UnpackedSize := Entry.Size;
      FFiles[FCount].PackMethod := pmStore;

      Inc(FCount);
    end;

    for i:=1 to Head.TableCount-1 do begin
      FFiles[i].Offset := FFiles[i-1].Offset + FFiles[i-1].PackedSize;
    end;
  except
  end;
end;

end.

