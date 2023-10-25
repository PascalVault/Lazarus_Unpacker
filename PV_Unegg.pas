unit PV_Unegg;

{$mode objfpc}{$H+}

//PV Unpack
//https://github.com/PascalVault
//Licence: MIT
//Last update: 2023-10-25
//Alzip .EGG

interface

uses
  Classes, SysUtils, PV_Unpack, Dialogs;

  { TUnPak }

type
  TUnegg = class(TUnpack)
  public
    constructor Create(Str: TStream); override;
  end;


implementation

{ TUntar }

constructor TUnegg.Create(Str: TStream);
type THead = packed record
       Magic: array[1..4] of Char;
       Version: Word;
       Random: Cardinal;
       Reserved: Cardinal;
       EOFARC: Cardinal;
       //18 bytes
     end;
     TFileHead = packed record
       //Magic: Cardinal;
       Unique: Cardinal;
       FileLen: Int64;
       //12 bytes+Magic
     end;
     TFilenameHead = packed record
       //Magic: Cardinal;
       BitFlag: Byte;
       Size: Word;
       //7 bytes
    end;
    TWindowsHead = packed record
      //Magic: Cardinal;
      BitFlag: Byte;
      Size: Word;
      LastModDate: Int64;
      Attr: Byte;
      EOFARC: Cardinal;
      //20 bytes
    end;
    TBlockHead = packed record
      //Magic: Cardinal;
      Method: Byte;
      Method2: Byte;
      UnpackedSize: Cardinal;
      PackedSize: Cardinal;
      CRC32: Cardinal;
      EOFARC: Cardinal;
      //22 bytes
    end;

var Head: THead;
    Magic: Cardinal;
    FileHead: TFileHead;
    FilenameHead: TFilenameHead;
    WindowsHead: TWindowsHead;
    BlockHead: TBlockHead;
    i: Integer;
    FName: String;
begin
  FStream := Str;
  FName := '';

  try
    FSize  := 1000;
    SetLength(FFiles, FSize);
    FCount := 0;

    FStream.Read(Head, SizeOf(Head));

    while FStream.Position < FStream.Size do begin

      FStream.Read(Magic, 4);

      if Magic = $08E28222 then begin
        break; //EOF
      end
      else if Magic = $0A8590E3 then begin
        FStream.Read(FileHead, SizeOf(FileHead));
      end
      else if Magic = $0A8591AC then begin
        FStream.Read(FilenameHead, SizeOf(FilenameHead));
        SetLength(FName, FilenameHead.Size);
        FStream.Read(FName[1], FilenameHead.Size);
      end
      else if Magic = $2C86950B then begin
        FStream.Read(WindowsHead, SizeOf(WindowsHead));
      end
      else if Magic = $02B50C13 then begin
        FStream.Read(BlockHead, SizeOf(BlockHead));

        if FCount = FSize then begin
          Inc(FSize, 1000);
          SetLength(FFiles, FSize);
        end;

        FFiles[FCount].Name := FName;
        FFiles[FCount].Offset := FStream.Position;
        FFiles[FCount].PackedSize := BlockHead.PackedSize;
        FFiles[FCount].UnpackedSize := BlockHead.UnpackedSize;

        if BlockHead.Method=0      then FFiles[FCount].PackMethod := pmStore
        else if BlockHead.Method=1 then FFiles[FCount].PackMethod := pmDeflate
        else if BlockHead.Method=2 then FFiles[FCount].PackMethod := pmBzip2
        else if BlockHead.Method=4 then FFiles[FCount].PackMethod := pmOther //pmLZMA. Seems it lacks the 5 prop bytes
        else                            FFiles[FCount].PackMethod := pmOther;

        Inc(FCount);

        FStream.Position := FStream.Position + BlockHead.PackedSize;
      end;
    end;
  except
  end;
end;

end.

