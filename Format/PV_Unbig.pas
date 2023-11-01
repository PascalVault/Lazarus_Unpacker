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
    AFile: TFile;
begin
  inherited Create(Str);

  FStream := Str;

  try
    FStream.Read(Head, SizeOf(Head));

    Head.ArchiveSize := SwapEndian(Head.ArchiveSize);
    Head.TableCount  := SwapEndian(Head.TableCount);
    Head.File1Offset := SwapEndian(Head.File1Offset);

    for i:=0 to Head.TableCount-1 do begin

      FStream.Read(Entry, SizeOf(Entry));
      FName := ReadStrNull(FStream);

      AFile.Name := FName;
      AFile.Offset := SwapEndian(Entry.Offset);
      AFile.PackedSize := SwapEndian(Entry.Size);
      AFile.UnpackedSize := SwapEndian(Entry.Size);
      AFile.PackMethod := pmStore;
      AFile.ModDate := 0;
      AFile.CRC32 := 0;

      AddFile(AFile);
    end;
  except
  end;
end;

end.

