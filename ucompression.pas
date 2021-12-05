unit UCompression;

{$mode objfpc}
{$H+}
{$IfNDef Debug}
  {$Inline On}
{$EndIf}

interface

uses
  Classes;

function Decode(const Input, Output: TStream): Boolean;

implementation

uses
  ULZMADecoder,
  ULZMACommon;

function Decode(const Input, Output: TStream): Boolean;
const
  PropertiesSize = 5;
var
  Decoder: TLZMADecoder;
  i, v: Byte;
  Properties: array[0..4] of Byte;
  OutputSize: Int64;
begin
  Result := False;
  if Input.Read(Properties, PropertiesSize) <> PropertiesSize then
    Exit;
  Decoder := TLZMADecoder.Create;
  try
    if not Decoder.SetDecoderProperties(Properties) then
      Exit;
    OutputSize := 0;
    for i := 0 to 7 do
    begin
      v := ReadByte(Input);
      OutputSize := OutputSize or v shl (8 * i);
    end;
    if Decoder.Code(Input, Output, OutputSize) then
      Result := True;
  finally
    Decoder.Free;
  end;
end;

end.
