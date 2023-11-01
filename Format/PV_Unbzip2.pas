unit PV_Unbzip2;

{$mode objfpc}{$H+}

//PV Unpack
//https://github.com/PascalVault
//Licence: MIT
//Last update: 2023-09-18
//BZip2 .BZ2

interface

uses
  Classes, SysUtils, PV_Unpack, Dialogs;

  { TUnPak }

type
  TUnbzip2 = class(TUnpack)
  public
    constructor Create(Str: TStream); override;
  end;


implementation

{ TUntar }

constructor TUnbzip2.Create(Str: TStream);
type THead = packed record
      Magic: array[0..2] of Char;
      BlockSize: Byte;
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
    AFile.Offset := FStream.Position-4;
    AFile.PackedSize := FStream.Size;
    AFile.UnpackedSize := 0;
    AFile.PackMethod := pmBzip2;
    AFile.ModDate := 0;
    AFile.CRC32 := 0;

    AddFile(AFile);

  except
  end;
end;

end.

