unit rle90;

{$mode objfpc}{$H+}

//PV Unpack
//https://github.com/PascalVault
//Licence: MIT
//Last update: 2023-11-11

interface

uses
  Classes, SysUtils;

type

  { TRLE90 }

  TRLE90 = class
  private
    FInStream: TStream;
    FOutStream: TStream;
  public
    constructor Create(InStream, OutStream: TStream);
    procedure Decode(PackedSize: Int64);
  end;

implementation

constructor TRLE90.Create(InStream, OutStream: TStream);
begin
  FInStream := InStream;
  FOutStream := OutStream;
end;

procedure TRLE90.Decode(PackedSize: Int64);
const NoHistory = 0;
      InLoop = 1;
var i, k: Integer;
    Code: Byte;
    C: Byte;
    LastC: Byte;
    state: Byte;
begin
  Code := $90;
  i := 0;
  state := NoHistory;

  while i < packedSize do begin
    FInStream.Read(C, 1);

    case state of
      NoHistory: begin
                   if C = $90 then state := InLoop
                   else begin
                     FOutStream.Write(C, 1);
                     LastC := C;
                   end;
                 end;
      InLoop:    begin
                   if c <> 0 then begin
                   for k:=0 to c-2 do
                     FOutStream.Write(LastC, 1);
                   end
                   else FOutStream.Write(Code, 1);
                   state := NoHistory;
                 end;
      else       raise Exception.Create('Invalid state');
    end;
    Inc(i);
  end;
end;

end.

