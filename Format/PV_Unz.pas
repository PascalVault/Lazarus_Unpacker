unit PV_Unz;

{$mode objfpc}{$H+}

//PV Unpack
//https://github.com/PascalVault
//Licence: MIT
//Last update: 2023-11-18
//Thom Henderson .ARC

interface

uses
  Classes, SysUtils, PV_Unpack, Dialogs;

  { TUnPak }

type
  TUnZ = class(TUnpack)
  public
    constructor Create(Str: TStream); override;
  end;


implementation

{ TUntar }

constructor TUnZ.Create(Str: TStream);
type THead = packed record
       Magic: Word; //1F,9D
       Method: Byte;
     end;

var Head: THead;
    i: Integer;
    AFile: TFile;
begin
  inherited Create(Str);

  FStream := Str;

  try
      FStream.Read(Head, SizeOf(Head));

      AFile.Name := '';
      AFile.Offset := FStream.Position;
      AFile.PackedSize := FStream.Size-3;
      AFile.UnpackedSize := 0;

      AFile.Extra := Head.Method ;//and $1F;
      showmessage(IntToStr(afile.extra));

      AFile.PackMethod := pmSquashLZW;

      AFile.ModDate := 0;
      AFile.CRC32 := 0;

      AddFile(AFile);

  except
  end;
end;

end.

