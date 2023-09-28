unit PV_Unbig;

{$mode objfpc}{$H+}

//PV Unpack
//https://github.com/PascalVault
//Licence: MIT
//Last update: 2023-09-18
//Electronic Arts .BIG

interface

uses
  Classes, SysUtils, PV_Unpack, Dialogs;

  { TUnPak }

type
  TUnbig = class(TUnpack)
  public
    constructor Create(Str: TStream); override;
  end;


implementation

{ TUntar }

constructor TUnbig.Create(Str: TStream);
type THead = packed record
       Magic: array[0..3] of Char; //BIGF or BIGH; one is big endian
       ArchiveSize: Cardinal;
       TableCount: Cardinal;
       File1Offset: Cardinal;
    end;
    TEntry = packed record
       Offset: Cardinal;
       Size: Cardinal;
       //FName: String; //null terminated
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

    Head.ArchiveSize := SwapEndian(Head.ArchiveSize);
    Head.TableCount  := SwapEndian(Head.TableCount);
    Head.File1Offset := SwapEndian(Head.File1Offset);

    for i:=0 to Head.TableCount-1 do begin

      FStream.Read(Entry, SizeOf(Entry));
      FName := ReadStrNull(FStream);

      if FCount = FSize then begin
        Inc(FSize, 1000);
        SetLength(FFiles, FSize);
      end;

      FFiles[FCount].Name := FName;
      FFiles[FCount].Offset := SwapEndian(Entry.Offset);
      FFiles[FCount].PackedSize := SwapEndian(Entry.Size);
      FFiles[FCount].UnpackedSize := SwapEndian(Entry.Size);
      FFiles[FCount].PackMethod := pmStore;

      Inc(FCount);
    end;
  except
  end;
end;

end.

