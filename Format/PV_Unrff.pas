unit PV_Unrff;

{$mode objfpc}{$H+}

//PV Unpack
//https://github.com/PascalVault
//Licence: MIT
//Last update: 2023-10-15
//Blood .RFF

interface

uses
  Classes, SysUtils, PV_Unpack, Dialogs;

  { TUnPak }

type
  TUnrff = class(TUnpack)
  public
    constructor Create(Str: TStream); override;
  end;


implementation

{ TUntar }

constructor TUnrff.Create(Str: TStream);
type THead = packed record
      Magic: array[0..3] of Char; //"RFF" + $1A
      Version: Word;
      Unknown: Word;
      Offset: Cardinal;
      Count: Cardinal;
      Padding: array[0..15] of Char;
    end;
    TEntry = packed record
      Buffer: array[0..15] of Char;
      Offset: Cardinal;
      UnpackedSize: Cardinal;
      PackedSize: Cardinal;
      DateTime: Cardinal; //unix stamp
      Flags: Byte;
      Ext: array[0..2] of Char;
      FName: array[0..7] of Char;
      FileID: Cardinal;
    end;

var Head: THead;
    Entry: TEntry;
    i: Integer;
    Enc: array of Byte;
    EncLen: Integer;
    Key: Byte;
    j: Integer;
    Mem: TMemoryStream;
    AFile: TFile;
begin
  inherited Create(Str);

  FStream := Str;

  try
    FStream.Read(Head, SizeOf(Head));

    FStream.Position := Head.Offset;

    //file index is encrypted
    EncLen := Head.Count * SizeOf(Entry);
    SetLength(Enc, EncLen);
    FStream.Read(Enc[0], EncLen);

    //decode
    if (Head.Version = 768) or (Head.Version = 769) then begin

        Key := Head.Offset and $FF;

        for j:=0 to EncLen-1 do begin
          if Head.Version = 768 then begin
             Enc[j] := Enc[j] xor (Key shr 1);
             Inc(Key);
          end
          else begin
             Enc[j] := Enc[j] xor Key;
             Inc(Key, j and 1);
          end;
        end;

    end;
    //===

    Mem := TMemoryStream.Create;
    Mem.Write(Enc[0], EncLen);
    Mem.Position := 0;

    for i:=0 to Head.Count-1 do begin

      Mem.Read(Entry, SizeOf(Entry));

      AFile.Name := Entry.FName + '.' +Entry.Ext;
      AFile.Offset := Entry.Offset;
      AFile.PackedSize := Entry.PackedSize;
      AFile.UnpackedSize := Entry.UnpackedSize;
      AFile.ModDate := Unix2DateTime(Entry.DateTime);
      AFile.CRC32 := 0;

      if Entry.Flags and $10 = $10 then
        AFile.PackMethod := pmRff
      else
        AFile.PackMethod := pmStore;

      AddFile(AFile);
    end;

    Mem.Free;
  except
  end;
end;

end.

