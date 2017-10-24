{****************************************************************************
Copyright (c) 2014 Randolph

mail: rilyu@sina.com

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in
all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
THE SOFTWARE.
****************************************************************************}

unit Jsons;

interface

uses Classes, SysUtils;

type
  //ÿ��Json��ֵ����ֵ�����ͣ�None��Null���ַ�������ֵ����������������
  TJsonValueType = (jvNone, jvNull, jvString, jvNumber, jvBoolean, jvObject, jvArray);
  //
  TJsonStructType = (jsNone, jsArray, jsObject);
  //
  TJsonNull = (null);
  //
  TJsonEmpty = (empty);

type
  //TJsonBase�Ǳ�Json������Ԫ��������Ļ���
  TJsonBase = class(TObject)
  private
    {Json�Ƕ��Ƕ�׵ĸ�ʽ���ܵ���˵������һ�����νṹ
      ���ݽṹ��ѧϰ�Ķ�����ʱ������ÿ���ڵ��ǣ�ֵ����ڵ㡢�ҽڵ�
      ����Json��Ӧ���������Ƕ�����������ÿ���ڵ������0��1��2��3��n���ӽڵ�
      ��ʵ�ܶ����鷴��˼���ܿ�����һ�������磺���������¿������Ǵ������Ͽ�
      ���Գ���õ������˸��ڵ��⣬����ÿ���ڵ㶼��һ�����ڵ㣬���ڵ�û�и��ڵ�
      ����Ԫ�е�Json��������ͨ�����򹹽����������ȥ�����ʹ��Json��}
    FOwner: TJsonBase;                     //���ĸ�Ԫ����Ϣ
    function GetOwner: TJsonBase;          //��ȡ���ĸ�Ԫ��

  protected
    function GetOwnerName: String;         //��ȡ��Ԫ�ص�����
    //�쳣��ط���
    procedure RaiseError(const Msg: String);
    procedure RaiseParseError(const JsonString: String);
    procedure RaiseAssignError(Source: TJsonBase);
    
  public
    constructor Create(AOwner: TJsonBase);  //���ݸ�Ԫ�ع���һ������
    destructor Destroy; override;           //�ͷŶ���

    //�涨�����ӿڣ����ಢ��ʵ�֣�ֻԼ���ӿڣ�����������ʵ��
    procedure Parse(JsonString: String); virtual; abstract;

    //�涨����ӿڣ����ಢ��ʵ�֣�ֻԼ���ӿڣ�����������ʵ��
    function Stringify: String; virtual; abstract;

    //Assign������ҪӦ����Put������TJsonBase�������֮��ĸ�ֵ����
    //��ΪNumber��string��Boolean���Ϳ���ֱ��ʹ�� := ���и�ֵ
    //TJsonObject��TJsonValue��TJsonArray����ֱ��ʹ�� := ��ֵ����ΪDelphiͨ���ඨ��Ķ�����ʵֻ��һ��ָ�룬:= Ҳֻ��ָ��ָ��ĵ�ַ�仯����������
    //�������µ�Put��������Ҫ���ø���TBase�����Assign������ֵ
    //Put(const Value: TJsonObject)��Put(const Value: TJsonValue)��Put(const Value: TJsonArray)
    procedure Assign(Source: TJsonBase); virtual; abstract;

    function Encode(const S: String): String;
    function Decode(const S: String): String;

    //���շָ�����Json�����յ�ǰ�Ĳ���зָ��ɶ�ݣ���ΪJson�Ƕ����ṹ�����԰��յ�ǰ��ָ��Ǻ���Ҫ�ģ�
    //Split�����ǽ���Json�ṹ�ַ����ĺܺ��ĵķ���
    procedure Split(const S: String; const Delimiter: Char; Strings: TStrings);

    //�ж�ֵ�ǲ���ĳ�ֽṹ
    function IsJsonObject(const S: String): Boolean;         //�ж�ֵ�ǲ���һ��Json����ṹ
    function IsJsonArray(const S: String): Boolean;          //�ж�ֵ�ǲ���һ��Json����ṹ
    function IsJsonString(const S: String): Boolean;         //�ж�ֵ�ǲ���һ��Json�ַ���
    function IsJsonNumber(const S: String): Boolean;         //�ж�ֵ�ǲ���һ��Json��ֵ
    function IsJsonBoolean(const S: String): Boolean;        //�ж�ֵ�ǲ���һ��Json����
    function IsJsonNull(const S: String): Boolean;           //�ж�ֵ�ǲ���һ��Json Null
    //�ж�ֵ������ʲô�ṹ
    function AnalyzeJsonValueType(const S: String): TJsonValueType;

  public
    property Owner: TJsonBase read GetOwner;
    
  end;

  //Json����ṹ��Ӧ����
  //Json����ṹ����{��ʼ����}��β
  TJsonObject = class;

  //Json����ṹ��Ӧ����
  //Json����ṹ����[��ʼ����]��β
  TJsonArray = class;

  //Json��ֵ�Խṹ��ֵ��Ӧ����
  //ֵ�����Ƕ���ṹ������������ṹ���������ַ�������������ֵ�������ǲ���������
  //��ע�⡿TJsonPair����һ��Json��ֵ�Խṹ���࣬��TJsonValueֻ�Ǽ�ֵ����ֵ��Ӧ����
  TJsonValue = class(TJsonBase)
  private
    FValueType: TJsonValueType;     //ÿ��Json��ֵ����ֵ�����ͣ�None��Null���ַ�������ֵ����������������
    FStringValue: String;           //�����ֵ�Ե�ֵ���ַ������ͣ���ô��Ӧ��ֵ������
    FNumberValue: Extended;         //�����ֵ�Ե�ֵ����ֵ���ͣ���ô��Ӧ��ֵ������
    FBooleanValue: Boolean;         //�����ֵ�Ե�ֵ�ǲ������ͣ���ô��Ӧ��ֵ������
    FObjectValue: TJsonObject;      //�����ֵ�Ե�ֵ�Ƕ���ṹ����ô��Ӧ��ֵ������
    FArrayValue: TJsonArray;        //�����ֵ�Ե�ֵ������ṹ����ô��Ӧ��ֵ������

    function GetAsBoolean: Boolean;     //��ȡ��ֵ�Ե�ֵ���Բ������ͻ�ȡֵ
    function GetAsInteger: Integer;     //��ȡ��ֵ�Ե�ֵ�������ͻ�ȡֵ
    function GetAsNumber: Extended;     //��ȡ��ֵ�Ե�ֵ������ֵ�ͻ�ȡ
    function GetAsArray: TJsonArray;    //��ȡ��ֵ�Ե�ֵ��������ṹ��ȡֵ
    function GetAsObject: TJsonObject;  //��ȡ��ֵ�Ե�ֵ���Զ���ṹ��ȡ
    function GetAsString: String;       //��ȡ��ֵ�Ե�ֵ�����ַ������ͻ�ȡ
    function GetIsNull: Boolean;        //��ȡ��ֵ�Ե�ֵ���ж����ǲ���NULL
    function GetIsEmpty: Boolean;       //��ȡ��ֵ�Ե�ֵ���ж���ʵ�ǲ���Empty
    
    procedure SetAsBoolean(const Value: Boolean);      //���ü�ֵ�Ե�ֵ����������
    procedure SetAsInteger(const Value: Integer);      //���ü�ֵ�Ե�ֵ������
    procedure SetAsNumber(const Value: Extended);      //���ü�ֵ�Ե�ֵ����ֵ
    procedure SetAsString(const Value: String);        //���ü�ֵ�Ե�ֵ���ַ���
    procedure SetAsArray(const Value: TJsonArray);     //���ü�ֵ�Ե�ֵ������ṹ
    procedure SetAsObject(const Value: TJsonObject);   //���ü�ֵ�Ե�ֵ������ṹ
    procedure SetIsNull(const Value: Boolean);         //���ü�ֵ�Ե�ֵ���ǲ���NULL
    procedure SetIsEmpty(const Value: Boolean);        //���ü�ֵ�Ե�ֵ���ǲ���Empty

  protected
    procedure RaiseValueTypeError(const AsValueType: TJsonValueType);

  public
    constructor Create(AOwner: TJsonBase);
    destructor Destroy; override;

    //ʵ�ָ��ඨ��Ľ���������������Ӧ�ļ�ֵ���ַ���
    //����һ����ֵ�Ե�ֵ���֣���������������ֵ or �ַ��� or ���� or ���� �������õ���Ӧ����ֵֵ or �ַ���ֵ or ����ֵ or ����ֵ ��
    procedure Parse(JsonString: String); override;

    //ʵ�ָ��ඨ��Ĵ������������õ���Ӧ�ļ�ֵ���ַ���
    //�������õ�ֵ����ֵ or �ַ��� or ���� or ���� ������Ӧ����õ���ֵ�� or �ַ����� or ��ֵ�� or ������ �����ַ�����
    function Stringify: String; override;

    //ʵ�ָ��ඨ��ĸ�ֵ������Source��Ӧ������TJsonValue��TJsonArray��TJsonObject
    procedure Assign(Source: TJsonBase); override;

    procedure Clear;

  public
    //��Ӧ��Set��Get
    property ValueType: TJsonValueType read FValueType;
    property AsString: String read GetAsString write SetAsString;
    property AsNumber: Extended read GetAsNumber write SetAsNumber;
    property AsInteger: Integer read GetAsInteger write SetAsInteger;
    property AsBoolean: Boolean read GetAsBoolean write SetAsBoolean;
    property AsObject: TJsonObject read GetAsObject write SetAsObject;
    property AsArray: TJsonArray read GetAsArray write SetAsArray;
    property IsNull: Boolean read GetIsNull write SetIsNull;
    property IsEmpty: Boolean read GetIsEmpty write SetIsEmpty;
    
  end;

  //Json����ṹ��Ӧ����
  //Json����ṹ����[��ʼ����]��β
  //����Json��ʽ�������ڲ������Ŀ������ַ�������ֵ�����������顢�������͵�ֵ
  //�ڱ����������ʵ���У�Json����������Ǵ�0�����Ǵ�1��ʼ��
  TJsonArray = class(TJsonBase)
  private
    //ʹ������洢����ṹ��ÿ��Ԫ�أ�Ԫ�ؿ�����TJsonArray��TJsonObject��TJsonValue
    //����˵��Json��ʽ��ʵ����һ���������ݽṹ����TJsonBase��ʹ��FOwner: TJsonBase;������ÿ���ڵ�ĸ��ڵ�
    //��ʵ��ʹ��������������ÿ���ڵ���ӽڵ㣡
    //����ֻ��TJsonArray��TJsonObject�����ӽڵ㣬��Ҫʹ��List�������ӽڵ�
    FList: TList;
    function GetItems(Index: Integer): TJsonValue;   //��ȡ����ṹ�ĵ�Index��Ԫ�أ�ÿ��Ԫ��ʱTJsonValue����
    function GetCount: Integer;                      //��ȡ����ṹ��Ԫ�صĸ���
    
  public
    constructor Create(AOwner: TJsonBase = nil);
    destructor Destroy; override;

    //ʵ�ָ��ඨ��Ľ���������������Ӧ������ṹ
    //���Parse������JsonString������Ӧ�������� [{key1:value1,key2:value2},{key3:value3}] �ṹ���ַ���
    procedure Parse(JsonString: String); override;

    //ʵ�ָ��ඨ��Ĵ������������õ���Ӧ������ṹ
    //���ݸ����ݽṹ�е�Ԫ�أ�����õ����� [{key1:value1,key2:value2},{key3:value3}] �ṹ���ַ���
    function Stringify: String; override;

    //ʵ�ָ��ඨ��ĸ�ֵ������Sourceֻ����TJsonArray����
    procedure Assign(Source: TJsonBase); override;

    //�ϲ���������ṹ��һ������ṹ��
    procedure Merge(Addition: TJsonArray);

    function Add: TJsonValue;                                     //������ṹ���ټ�һ��Ԫ��
    function Insert(const Index: Integer): TJsonValue;            //

    //����������Ӷ�Ӧ���͵�Ԫ�أ��ڲ�ͨ������Add����ʵ��
    function Put(const Value: TJsonEmpty): TJsonValue; overload;  //���һ��Empty���͵�Ԫ��
    function Put(const Value: TJsonNull): TJsonValue; overload;   //���һ��Null���͵�Ԫ��
    function Put(const Value: Boolean): TJsonValue; overload;     //���һ���������͵�Ԫ��
    function Put(const Value: Integer): TJsonValue; overload;     //���һ�����͵�Ԫ��
    function Put(const Value: Extended): TJsonValue; overload;    //���һ����ֵ�͵�Ԫ��
    function Put(const Value: String): TJsonValue; overload;      //���һ���ַ����͵�Ԫ��
    function Put(const Value: TJsonArray): TJsonValue; overload;  //���һ������ṹ��Ԫ��
    function Put(const Value: TJsonObject): TJsonValue; overload; //���һ������ṹ��Ԫ��
    function Put(const Value: TJsonValue): TJsonValue; overload;  //���һ����ֵ�Ե�Ԫ��

    procedure Delete(const Index: Integer);                       //ɾ����Index��Ԫ��
    procedure Clear;
    
  public
    property Count: Integer read GetCount;                             //��ȡ������Ԫ�صĸ���
    property Items[Index: Integer]: TJsonValue read GetItems; default; //����������ȡ��Index��Ԫ��              
  end;

  //Json��ֵ�Խṹ��Ӧ����
  //����string���ͣ�ֵ��TJsonValue����
  //����Json��ʽ��ֵ�������ַ������͡���ֵ�͡������͡�����ṹ������ṹ
  TJsonPair = class(TJsonBase)
  private
    FName: String;        //һ����ֵ�Եļ�
    FValue: TJsonValue;   //һ����ֵ�Ե�ֵ

    procedure SetName(const Value: String);   //����һ����ֵ�Եļ�
    
  public
    constructor Create(AOwner: TJsonBase; const AName: String = '');
    destructor Destroy; override;

    //���Parse�����Ĳ��������� key:value �ṹ���ַ������÷����������ַ�����ȡ��Ӧ�ļ���ֵ
    procedure Parse(JsonString: String); override;

    //���ݼ���ֵ������õ����� key:value �ṹ���ַ���
    function Stringify: String; override;

    //ʵ�ָ��ඨ��ĸ�ֵ������Sourceֻ����TJsonPair����
    procedure Assign(Source: TJsonBase); override;
    
  public
    property Name: String read FName write SetName;
    property Value: TJsonValue read FValue;
    
  end;

  //Json����ṹ��Ӧ����
  //Json����ṹ����{��ʼ����}��β
  //����Json��ʽ������ṹ�ڲ��ɶ����ֵ�����
  TJsonObject = class(TJsonBase)
  private
    //ʹ������洢����ṹ��ÿ��Ԫ�أ�Ԫ�ؿ�����TJsonArray��TJsonObject��TJsonValue
    //����˵��Json��ʽ��ʵ����һ���������ݽṹ����TJsonBase��ʹ��FOwner: TJsonBase;������ÿ���ڵ�ĸ��ڵ�
    //��ʵ��ʹ��������������ÿ���ڵ���ӽڵ㣡
    //����ֻ��TJsonArray��TJsonObject�����ӽڵ㣬��Ҫʹ��List�������ӽڵ�
    FList: TList;
    FAutoAdd: Boolean;
    function GetCount: Integer;
    function GetItems(Index: Integer): TJsonPair;
    function GetValues(Name: String): TJsonValue;
    
  public
    constructor Create(AOwner: TJsonBase = nil);
    destructor Destroy; override;

    //ʵ�ָ��ඨ��Ľ���������������Ӧ�Ķ���ṹ
    //���Parse������JsonString������Ӧ�������� {key1:value1,key2:value2} �ṹ���ַ���
    procedure Parse(JsonString: String); override;

    //ʵ�ָ��ඨ��Ĵ������������õ���Ӧ�Ķ���ṹ
    //���ݸ����ݽṹ�е�Ԫ�أ�����õ����� {key1:value1,key2:value2} �ṹ���ַ���
    function Stringify: String; override;

    //ʵ�ָ��ඨ��ĸ�ֵ������Sourceֻ����TJsonObject����
    procedure Assign(Source: TJsonBase); override;

    //�ϲ���������ṹΪһ������ṹ
    procedure Merge(Addition: TJsonObject);

    function Add(const Name: String = ''): TJsonPair;
    function Insert(const Index: Integer; const Name: String = ''): TJsonPair;

    function Put(const Name: String; const Value: TJsonEmpty): TJsonValue; overload;
    function Put(const Name: String; const Value: TJsonNull): TJsonValue; overload;
    function Put(const Name: String; const Value: Boolean): TJsonValue; overload;
    function Put(const Name: String; const Value: Integer): TJsonValue; overload;
    function Put(const Name: String; const Value: Extended): TJsonValue; overload;
    function Put(const Name: String; const Value: String): TJsonValue; overload;
    function Put(const Name: String; const Value: TJsonArray): TJsonValue; overload;
    function Put(const Name: String; const Value: TJsonObject): TJsonValue; overload;
    function Put(const Name: String; const Value: TJsonValue): TJsonValue; overload;
    function Put(const Value: TJsonPair): TJsonValue; overload;

    function Find(const Name: String): Integer;

    procedure Delete(const Index: Integer); overload;
    procedure Delete(const Name: String); overload;

    procedure Clear;
    
  public
    property Count: Integer read GetCount;
    property Items[Index: Integer]: TJsonPair read GetItems;            //�÷� XXX[i].
    property Values[Name: String]: TJsonValue read GetValues; default;  //�÷� XXX['S'].
    property AutoAdd: Boolean read FAutoAdd write FAutoAdd;
    
  end;

  //�����ڴ���õ�һ��������Json�ַ��������һ��������Json�ַ���ʱ��Ҫʹ��TJson��
  TJson = class(TJsonBase)
  private
    FStructType: TJsonStructType;
    FJsonArray: TJsonArray;
    FJsonObject: TJsonObject;

    function GetCount: Integer;
    function GetJsonArray: TJsonArray;
    function GetJsonObject: TJsonObject;
    function GetValues(Name: String): TJsonValue;
    
  protected
    procedure CreateArrayIfNone;
    procedure CreateObjectIfNone;

    procedure RaiseIfNone;
    procedure RaiseIfNotArray;
    procedure RaiseIfNotObject;

    procedure CheckJsonArray;
    procedure CheckJsonObject;

  public
    constructor Create;
    destructor Destroy; override;

    //����һ��������Json�ַ��������Parse�Ĳ���JsonString��һ��������Json�ַ���
    procedure Parse(JsonString: String); override;

    //����õ�һ��������Json�ַ���
    function Stringify: String; override;

    //ʵ�ָ��ඨ��ĸ�ֵ������Source�������������ͣ�TJson��TJsonArray��TJsonObject��TJsonValue
    procedure Assign(Source: TJsonBase); override;

    procedure Delete(const Index: Integer); overload;
    procedure Delete(const Name: String); overload;

    procedure Clear;

    function Get(const Index: Integer): TJsonValue; overload;   //for both
    function Get(const Name: String): TJsonValue; overload;     //for JsonObject

    //���������ṹʹ��
    function Put(const Value: TJsonEmpty): TJsonValue; overload;
    function Put(const Value: TJsonNull): TJsonValue; overload;
    function Put(const Value: Boolean): TJsonValue; overload;
    function Put(const Value: Integer): TJsonValue; overload;
    function Put(const Value: Extended): TJsonValue; overload;
    function Put(const Value: String): TJsonValue; overload;
    function Put(const Value: TJsonArray): TJsonValue; overload;
    function Put(const Value: TJsonObject): TJsonValue; overload;
    function Put(const Value: TJsonValue): TJsonValue; overload;
    function Put(const Value: TJson): TJsonValue; overload;

    //�������ͨ�ļ�ֵ��ʹ��
    function Put(const Name: String; const Value: TJsonEmpty): TJsonValue; overload;
    function Put(const Name: String; const Value: TJsonNull): TJsonValue; overload;
    function Put(const Name: String; const Value: Boolean): TJsonValue; overload;
    function Put(const Name: String; const Value: Integer): TJsonValue; overload;
    function Put(const Name: String; const Value: Extended): TJsonValue; overload;
    function Put(const Name: String; const Value: String): TJsonValue; overload;
    function Put(const Name: String; const Value: TJsonArray): TJsonValue; overload;
    function Put(const Name: String; const Value: TJsonObject): TJsonValue; overload;
    function Put(const Name: String; const Value: TJsonValue): TJsonValue; overload;
    function Put(const Name: String; const Value: TJson): TJsonValue; overload;
    function Put(const Value: TJsonPair): TJsonValue; overload;

  public
    property StructType: TJsonStructType read FStructType;
    property JsonObject: TJsonObject read GetJsonObject;
    property JsonArray: TJsonArray read GetJsonArray;

    property Count: Integer read GetCount;
    property Values[Name: String]: TJsonValue read GetValues; default; //for JsonObject

  end;

implementation

{ TJsonBase }

//�ж�ֵ������ʲô�ṹ
function TJsonBase.AnalyzeJsonValueType(const S: String): TJsonValueType;
var
  Len: Integer;
  Number: Extended;
begin
  Result := jvNone;
  Len := Length(S);
  //�������顢�ַ�����null�����������ȱ�Ȼ�����ڵ���2
  if Len >= 2 then
  begin
    //�����{��ͷ��}��β����ô�Ƕ���ṹ
    if (S[1] = '{') and (S[Len] = '}') then
    begin
      Result := jvObject
    end
    //�����[��ͷ��]��β����ô������ṹ
    else if (S[1] = '[') and (S[Len] = ']') then
    begin
      Result := jvArray
    end
    //�����"��ͷ��"��β����ô���ַ���
    else if (S[1] = '"') and (S[Len] = '"') then
    begin
      Result := jvString
    end
    //���ֵ��null����ô��null
    else if SameText(S, 'null') then
    begin
      Result := jvNull
    end
    //���ֵ��true��false����ô�ǲ���
    else if SameText(S, 'true') or SameText(S, 'false') then
    begin
      Result := jvBoolean
    end
    //���ֵ����ת����Float����ô����ֵ
    else if TryStrToFloat(S, Number) then
    begin
      Result := jvNumber;
    end;
  end
  //��ֵ������һ�����ڵ���2
  else if TryStrToFloat(S, Number) then
  begin
    Result := jvNumber;
  end;
end;

constructor TJsonBase.Create(AOwner: TJsonBase);
begin
  FOwner := AOwner;
end;

function TJsonBase.Decode(const S: String): String;

  function HexValue(C: Char): Byte;
  begin
    case C of
      '0'..'9':  Result := Byte(C) - Byte('0');
      'a'..'f':  Result := (Byte(C) - Byte('a')) + 10;
      'A'..'F':  Result := (Byte(C) - Byte('A')) + 10;
    else
      raise Exception.Create('Illegal hexadecimal characters "' + C + '"');
    end;
  end;

  function HexToUnicode(Hex: String): String;
  begin
    try
      Result := Char((HexValue(Hex[3]) shl 4) + HexValue(Hex[4]))
              + Char((HexValue(Hex[1]) shl 4) + HexValue(Hex[2]));
    except
      raise Exception.Create('Illegal four-hex-digits "' + Hex + '"');
    end;
  end;

var
  I: Integer;
  C: Char;
begin
  Result := '';
  I := 1;
  while I <= Length(S) do
  begin
    C := S[I];
    Inc(I);
    if C = '\' then
    begin
      C := S[I];
      Inc(I);
      case C of
        'b': Result := Result + #8;
        't': Result := Result + #9;
        'n': Result := Result + #10;
        'f': Result := Result + #12;
        'r': Result := Result + #13;
        'u':
          begin
            Result := Result + HexToUnicode(Copy(S, I, 4));
            Inc(I, 4);
          end;
      else
        Result := Result + C;
      end;
    end
    else
    begin
      Result := Result + C;
    end;
  end;
end;

destructor TJsonBase.Destroy;
begin
  inherited Destroy;
end;

function TJsonBase.Encode(const S: String): String;
var
  I: Integer;
  C: Char;
begin
  Result := '';
  for I := 1 to Length(S) do
  begin
    C := S[I];
    case C of
      '"', '\', '/': Result := Result + '\' + C;
      #8: Result := Result + '\b';
      #9: Result := Result + '\t';
      #10: Result := Result + '\n';
      #12: Result := Result + '\f';
      #13: Result := Result + '\r';
    else
      Result := Result + C;
    end;
  end;
end;

function TJsonBase.GetOwner: TJsonBase;
begin
  Result := FOwner;
end;

function TJsonBase.GetOwnerName: String;
var
  TheOwner: TJsonBase;
begin
  Result := '';
  TheOwner := Owner;
  while True do
  begin
    if not Assigned(TheOwner) then
    begin
      Break
    end
    else if TheOwner is TJsonPair then
    begin
      Result := (TheOwner as TJsonPair).Name;
      Break;
    end
    else
    begin
      TheOwner := TheOwner.Owner;
    end;
  end;
end;

//�ж�ֵ�ǲ���һ�� Json����ṹ
//Json������ص��ǣ��ԡ�{����ͷ���ԡ�}����β
function TJsonBase.IsJsonObject(const S: String): Boolean;
var
  Len: Integer;
begin
  Len := Length(S);
  Result := (Len >= 2) and (S[1] = '{') and (S[Len] = '}');
end;

//�ж�ֵ�ǲ���һ�� Json����ṹ
//Json������ص��ǣ��ԡ�[����ͷ���ԡ�]����β
function TJsonBase.IsJsonArray(const S: String): Boolean;
var
  Len: Integer;
begin
  Len := Length(S);
  Result := (Len >= 2) and (S[1] = '[') and (S[Len] = ']');
end;

//�ж�ֵ�ǲ��� Json����
//Json�������ص���ֵ��true��false
function TJsonBase.IsJsonBoolean(const S: String): Boolean;
begin
  Result := SameText(S, 'true') or SameText(S, 'false');
end;

//�ж�ֵ�ǲ��� Json Null
//Json Null���ص���ֵ��null
function TJsonBase.IsJsonNull(const S: String): Boolean;
begin
  Result := SameText(S, 'null');
end;

//�ж�ֵ�ǲ��� Json��ֵ
//Json��ֵ���ص��ǿ���ת����Float����
function TJsonBase.IsJsonNumber(const S: String): Boolean;
var
  Number: Extended;
begin
  Result := TryStrToFloat(S, Number);
end;

//�ж�ֵ�ǲ���һ��Json�ַ���
//Json�ַ������ص��ǣ��ԡ�"����ͷ���ԡ�"����β
function TJsonBase.IsJsonString(const S: String): Boolean;
var
  Len: Integer;
begin
  Len := Length(S);
  Result := (Len >= 2) and (S[1] = '"') and (S[Len] = '"');
end;

procedure TJsonBase.RaiseAssignError(Source: TJsonBase);
var
  SourceClassName: String;
begin
  if Source is TObject then
  begin
    SourceClassName := Source.ClassName
  end
  else
  begin
    SourceClassName := 'nil';
  end;
  RaiseError(Format('assign error: %s to %s', [SourceClassName, ClassName]));
end;

procedure TJsonBase.RaiseError(const Msg: String);
var
  S: String;
begin
  S := Format('<%s>%s', [ClassName, Msg]);
  raise Exception.Create(S);
end;

procedure TJsonBase.RaiseParseError(const JsonString: String);
begin
  RaiseError(Format('"%s" parse error: %s', [GetOwnerName, JsonString]));
end;

//����{key1:value1,key2:value2},{key3:value3} �õ� {key1:value1,key2:value2} �� {key3:value3}
//  ������{key1:value1 �� key2:value2} �� {key3:value3}
//  Splite�����л���ݡ�{��ȥƥ�䡰}����Ȼ���ж�ֻ������һ��
procedure TJsonBase.Split(const S: String; const Delimiter: Char; Strings: TStrings);

  function IsPairBegin(C: Char): Boolean;
  begin
    Result := (C = '{') or (C = '[') or (C = '"');
  end;

  //��ȡ�ַ�����һ��Ӧ���ַ�
  function GetPairEnd(C: Char): Char;
  begin
    case C of
      '{': Result := '}';
      '[': Result := ']';
      '"': Result := '"';
    else
      Result := #0;
    end;
  end;

  //���統ǰָ��P ָ��'{'����ôͨ�����ø÷����ҵ�'{'��Ӧ��'}'���ڵĵ�ַ����Ϊ����ֵ���أ�
  function MoveToPair(P: PChar): PChar;
  var
    PairBegin, PairEnd: Char;
    C: Char;
  begin
    PairBegin := P^;
    PairEnd := GetPairEnd(PairBegin);
    Result := P;
    while Result^ <> #0 do
    begin
      Inc(Result);
      C := Result^;
      if C = PairEnd then
      begin
        Break
      end
      else if (PairBegin = '"') and (C = '\') then
      begin
        Inc(Result)
      end
      else if (PairBegin <> '"') and IsPairBegin(C) then
      begin
        Result := MoveToPair(Result);
      end;
    end;
  end;

var
  PtrBegin, PtrEnd: PChar;
  C: Char;
  StrItem: String;
begin
  //����S='{key1:value1,key2:value2},{key3:value3}'
  PtrBegin := PChar(S);
  PtrEnd := PtrBegin;

  //��ѯ�ַ�������
  while PtrEnd^ <> #0 do
  begin
    C := PtrEnd^;

    //��һ��C='{'����else if ����else if�е��� MoveToPair����ָ�������'{'һ���Ӧ��'}'����Ȼ���������
    //�����Ϳ��԰���','�ֽ�Json�ַ���ʱֻ�ֽ⵱ǰ������㣬�������ڲ��','Ӱ�죡
    if C = Delimiter then
    begin
      StrItem := Trim(Copy(PtrBegin, 1, PtrEnd - PtrBegin));
      Strings.Add(StrItem);
      PtrBegin := PtrEnd + 1;
      PtrEnd := PtrBegin;
      Continue;
    end
    //������Ƿָ���������{��[��"
    else if IsPairBegin(C) then
    begin
      PtrEnd := MoveToPair(PtrEnd);
    end;
    Inc(PtrEnd);
  end;
  StrItem := Trim(Copy(PtrBegin, 1, PtrEnd - PtrBegin));
  
  if StrItem <> '' then
  begin
    Strings.Add(StrItem);
  end;
end;

{ TJsonValue }

procedure TJsonValue.Assign(Source: TJsonBase);
var
  Src: TJsonValue;
begin
  Clear;
  if not(Source is TJsonValue) and not(Source is TJsonObject) and not(Source is TJsonArray) then
  begin
    RaiseAssignError(Source);
  end;
  if Source is TJsonObject then
  begin
    FValueType := jvObject;
    FObjectValue := TJsonObject.Create(Self);
    FObjectValue.Assign(Source);
  end
  else if Source is TJsonArray then
  begin
    FValueType := jvArray;
    FArrayValue := TJsonArray.Create(Self);
    FArrayValue.Assign(Source);
  end
  else if Source is TJsonValue then
  begin
    Src := Source as TJsonValue;
    FValueType := Src.FValueType;
    case FValueType of
      jvNone, jvNull: ;
      jvString: FStringValue := Src.FStringValue;
      jvNumber: FNumberValue := Src.FNumberValue;
      jvBoolean: FBooleanValue := Src.FBooleanValue;
      jvObject:
        begin
          FObjectValue := TJsonObject.Create(Self);
          FObjectValue.Assign(Src.FObjectValue);
        end;
      jvArray:
        begin
          FArrayValue := TJsonArray.Create(Self);
          FArrayValue.Assign(Src.FArrayValue);
        end;
    end;
  end;
end;

procedure TJsonValue.Clear;
begin
  case FValueType of
    jvNone, jvNull: ;
    jvString: FStringValue := '';
    jvNumber: FNumberValue := 0;
    jvBoolean: FBooleanValue := False;
    jvObject:
      begin
        FObjectValue.Free;
        FObjectValue := nil;
      end;
    jvArray:
      begin
        FArrayValue.Free;
        FArrayValue := nil;
      end;
  end;
  FValueType := jvNone;
end;

constructor TJsonValue.Create(AOwner: TJsonBase);
begin
  inherited Create(AOwner);
  FStringValue := '';
  FNumberValue := 0;
  FBooleanValue := False;
  FObjectValue := nil;
  FArrayValue := nil;
  FValueType := jvNone;
end;

destructor TJsonValue.Destroy;
begin
  Clear;
  inherited Destroy;
end;

function TJsonValue.GetAsArray: TJsonArray;
begin
  if IsEmpty then
  begin
    FValueType := jvArray;
    FArrayValue := TJsonArray.Create(Self);
  end;
  if FValueType <> jvArray then
  begin
    RaiseValueTypeError(jvArray);
  end;
  Result := FArrayValue;
end;

function TJsonValue.GetAsBoolean: Boolean;
begin
  Result := False;
  case FValueType of
    jvNone, jvNull: Result := False;
    jvString: Result := SameText(FStringValue, 'true');
    jvNumber: Result := (FNumberValue <> 0);
    jvBoolean: Result := FBooleanValue;
    jvObject, jvArray: RaiseValueTypeError(jvBoolean);
  end;
end;

function TJsonValue.GetAsInteger: Integer;
begin
  Result := 0;
  case FValueType of
    jvNone, jvNull: Result := 0;
    jvString: Result := Trunc(StrToFloat(FStringValue));
    jvNumber: Result := Trunc(FNumberValue);
    jvBoolean: Result := Ord(FBooleanValue);
    jvObject, jvArray: RaiseValueTypeError(jvNumber);
  end;
end;

function TJsonValue.GetAsNumber: Extended;
begin
  Result := 0;
  case FValueType of
    jvNone, jvNull: Result := 0;
    jvString: Result := StrToFloat(FStringValue);
    jvNumber: Result := FNumberValue;
    jvBoolean: Result := Ord(FBooleanValue);
    jvObject, jvArray: RaiseValueTypeError(jvNumber);
  end;
end;

function TJsonValue.GetAsObject: TJsonObject;
begin
  if IsEmpty then
  begin
    FValueType := jvObject;
    FObjectValue := TJsonObject.Create(Self);
  end;
  if FValueType <> jvObject then
  begin
    RaiseValueTypeError(jvObject);
  end;
  Result := FObjectValue;
end;

function TJsonValue.GetAsString: String;
const
  BooleanStr: array[Boolean] of String = ('false', 'true');
begin
  Result := '';
  case FValueType of
    jvNone, jvNull: Result := '';
    jvString: Result := FStringValue;
    jvNumber: Result := FloatToStr(FNumberValue);
    jvBoolean: Result := BooleanStr[FBooleanValue];
    jvObject, jvArray: RaiseValueTypeError(jvString);
  end;
end;

function TJsonValue.GetIsEmpty: Boolean;
begin
  Result := (FValueType = jvNone);
end;

function TJsonValue.GetIsNull: Boolean;
begin
  Result := (FValueType = jvNull);
end;

procedure TJsonValue.Parse(JsonString: String);
begin
  Clear;
  FValueType := AnalyzeJsonValueType(JsonString);
  case FValueType of
    jvNone: RaiseParseError(JsonString);
    jvNull: ;
    jvString: FStringValue := Decode(Copy(JsonString, 2, Length(JsonString) - 2));
    jvNumber: FNumberValue := StrToFloat(JsonString);
    jvBoolean: FBooleanValue := SameText(JsonString, 'true');
    jvObject:
      begin
        FObjectValue := TJsonObject.Create(Self);
        FObjectValue.Parse(JsonString);
      end;
    jvArray:
      begin
        FArrayValue := TJsonArray.Create(Self);
        FArrayValue.Parse(JsonString);
      end;
  end;
end;

procedure TJsonValue.RaiseValueTypeError(const AsValueType: TJsonValueType);
const
  StrJsonValueType: array[TJsonValueType] of String = ('jvNone', 'jvNull', 'jvString', 'jvNumber', 'jvBoolean', 'jvObject', 'jvArray');
var
  S: String;
begin
  S := Format('"%s" value type error: %s to %s', [GetOwnerName, StrJsonValueType[FValueType], StrJsonValueType[AsValueType]]);
  RaiseError(S);
end;

procedure TJsonValue.SetAsArray(const Value: TJsonArray);
begin
  if FValueType <> jvArray then
  begin
    Clear;
    FValueType := jvArray;
    FArrayValue := TJsonArray.Create(Self);
  end;
  FArrayValue.Assign(Value);
end;

procedure TJsonValue.SetAsBoolean(const Value: Boolean);
begin
  if FValueType <> jvBoolean then
  begin
    Clear;
    FValueType := jvBoolean;
  end;
  FBooleanValue := Value;
end;

procedure TJsonValue.SetAsInteger(const Value: Integer);
begin
  SetAsNumber(Value);
end;

procedure TJsonValue.SetAsNumber(const Value: Extended);
begin
  if FValueType <> jvNumber then
  begin
    Clear;
    FValueType := jvNumber;
  end;
  FNumberValue := Value;
end;

procedure TJsonValue.SetAsObject(const Value: TJsonObject);
begin
  if FValueType <> jvObject then
  begin
    Clear;
    FValueType := jvObject;
    FObjectValue := TJsonObject.Create(Self);
  end;
  FObjectValue.Assign(Value);
end;

procedure TJsonValue.SetAsString(const Value: String);
begin
  if FValueType <> jvString then
  begin
    Clear;
    FValueType := jvString;
  end;
  FStringValue := Value;
end;

procedure TJsonValue.SetIsEmpty(const Value: Boolean);
const
  EmptyValueType: array[Boolean] of TJsonValueType = (jvNull, jvNone);
begin
  if FValueType <> EmptyValueType[Value] then
  begin
    Clear;
    FValueType := EmptyValueType[Value];
  end;
end;

procedure TJsonValue.SetIsNull(const Value: Boolean);
const
  NullValueType: array[Boolean] of TJsonValueType = (jvNone, jvNull);
begin
  if FValueType <> NullValueType[Value] then
  begin
    Clear;
    FValueType := NullValueType[Value];
  end;
end;

//Json���ṹ�м�ֵ�Ե�ֵ�Ĵ������
function TJsonValue.Stringify: String;
const
  StrBoolean: array[Boolean] of String = ('false', 'true');
begin
  Result := '';
  case FValueType of
    //none��null��ֱ�Ӹ�ֵnull
    jvNone, jvNull:
      Result := 'null';
    //�ַ������ͣ�ֱ�����ַ���ֵǰ��� "
    jvString:
      Result := '"' + Encode(FStringValue) + '"';
    //��ֵ���ͣ�ֱ�ӽ���ֵת�����ַ�����ʽ
    jvNumber:
      Result := FloatToStr(FNumberValue);
    //��������
    jvBoolean:
      Result := StrBoolean[FBooleanValue];
    //ֵ��������Json����
    jvObject:
      Result := FObjectValue.Stringify;
    //ֵ��������Json����
    jvArray:
      Result := FArrayValue.Stringify;
  end;
end;

{ TJsonArray }

function TJsonArray.Add: TJsonValue;
begin
  Result := TJsonValue.Create(Self);
  FList.Add(Result);
end;

procedure TJsonArray.Assign(Source: TJsonBase);
var
  Src: TJsonArray;
  I: Integer;
begin
  Clear;
  if not(Source is TJsonArray) then
  begin
    RaiseAssignError(Source);
  end;
  Src := Source as TJsonArray;
  for I := 0 to Src.Count - 1 do
  begin
    Add.Assign(Src[I]);
  end;
end;

procedure TJsonArray.Clear;
var
  I: Integer;
  Item: TJsonValue;
begin
  for I := 0 to FList.Count - 1 do
  begin
    Item := FList[I];
    Item.Free;
  end;
  FList.Clear;
end;

constructor TJsonArray.Create(AOwner: TJsonBase);
begin
  inherited Create(AOwner);
  FList := TList.Create;
end;

procedure TJsonArray.Delete(const Index: Integer);
var
  Item: TJsonValue;
begin
  Item := FList[Index];
  Item.Free;
  FList.Delete(Index);
end;

destructor TJsonArray.Destroy;
begin
  Clear;
  FList.Free;
  inherited;
end;

function TJsonArray.GetCount: Integer;
begin
  Result := FList.Count;
end;

function TJsonArray.GetItems(Index: Integer): TJsonValue;
begin
  Result := FList[Index];
end;

function TJsonArray.Insert(const Index: Integer): TJsonValue;
begin
  Result := TJsonValue.Create(Self);
  FList.Insert(Index, Result);
end;

procedure TJsonArray.Merge(Addition: TJsonArray);
var
  I: Integer;
begin
  for I := 0 to Addition.Count - 1 do
  begin
    Add.Assign(Addition[I]);
  end;
end;

//�������� [{key1:value1,key2:value2},{key3:value3},123] �ṹ���ַ���
procedure TJsonArray.Parse(JsonString: String);
var
  I: Integer;
  S: String;
  List: TStringList;
  Item: TJsonValue;
begin
  Clear;
  JsonString := Trim(JsonString);

  //���жϣ����JsonString��������ṹ���ַ�����ֱ���׳��쳣��������
  if not IsJsonArray(JsonString) then
  begin
    RaiseParseError(JsonString);
  end;

  //������ [{key1:value1,key2:value2},{key3:value3}] ���ַ���ת���� {key1:value1,key2:value2},{key3:value3}
  S := Trim(Copy(JsonString, 2, Length(JsonString) - 2));

  //ʹ��
  List := TStringList.Create;
  try
    //Split����ͬ�����ڸõ�Ԫ��ʵ�ֵ�
    //���������{key1:value1,key2:value2},{key3:value3}
    //ֻ�ǶԵ�һ����н������õ�{key1:value1,key2:value2} �� {key3:value3}
    //������{key1:value1 �� key2:value2} �� {key3:value3}
    //�б�Ҫȥ��һ���������������ڶ��ṹ������½��н�����
    //��������Ǳ���ԪԴ����һ���ȽϺ��ĵķ���
    Split(S, ',', List);

    //���ò������ֵ������������Ȼ�������ֵ��������ײ���н���
    //�����ڶ������������������
    //Json���Ƕ������νṹ��������ԪJson������ʵ���ж���Ľ�����������һ����֧���������һ�㣬Ȼ���ٽ�����һ����֧��
    //Ҳ����Parse�ڲ��ڵ�����һ���Parse��Ȼ���ٵ�����һ���Parse��ֱ�����õ����һ�㣬Ȼ����ȥ������һ����֧����
    for I := 0 to List.Count - 1 do
    begin
      Item := Add;
      Item.Parse(List[I]);
    end;
  finally
    List.Free;
  end;
end;

function TJsonArray.Put(const Value: Boolean): TJsonValue;
begin
  Result := Add;
  Result.AsBoolean := Value;
end;

function TJsonArray.Put(const Value: Integer): TJsonValue;
begin
  Result := Add;
  Result.AsInteger := Value;
end;

function TJsonArray.Put(const Value: TJsonEmpty): TJsonValue;
begin
  Result := Add;
  Result.IsEmpty := True;
end;

function TJsonArray.Put(const Value: TJsonNull): TJsonValue;
begin
  Result := Add;
  Result.IsNull := True;
end;

function TJsonArray.Put(const Value: Extended): TJsonValue;
begin
  Result := Add;
  Result.AsNumber := Value;
end;

function TJsonArray.Put(const Value: TJsonObject): TJsonValue;
begin
  Result := Add;
  Result.Assign(Value);
end;

function TJsonArray.Put(const Value: TJsonValue): TJsonValue;
begin
  Result := Add;
  Result.Assign(Value);
end;

function TJsonArray.Put(const Value: String): TJsonValue;
begin
  Result := Add;
  Result.AsString := Value;
end;

function TJsonArray.Put(const Value: TJsonArray): TJsonValue;
begin
  Result := Add;
  Result.Assign(Value);
end;

//��һ������ṹ���д��
function TJsonArray.Stringify: String;
var
  I: Integer;
  Item: TJsonValue;
begin
  Result := '[';
  for I := 0 to FList.Count - 1 do
  begin
    Item := FList[I];
    if I > 0 then
    begin
      Result := Result + ',';
    end;

    //�����ڲ���Ԫ�ض��Ǽ�ֵ�Ե�ֵ�����Էֱ����TJsonValue�Ĵ������
    //TJsonArray��ʹ������FList�洢�����ֵԪ��
    Result := Result + Item.Stringify;
  end;
  Result := Result + ']';
end;

{ TJsonPair }

procedure TJsonPair.Assign(Source: TJsonBase);
var
  Src: TJsonPair;
begin
  if not(Source is TJsonPair) then
  begin
    RaiseAssignError(Source);
  end;
  Src := Source as TJsonPair;
  FName := Src.FName;
  FValue.Assign(Src.FValue);
end;

constructor TJsonPair.Create(AOwner: TJsonBase; const AName: String);
begin
  inherited Create(AOwner);
  FName := AName;
  FValue := TJsonValue.Create(Self);
end;

destructor TJsonPair.Destroy;
begin
  FValue.Free;
  inherited Destroy;
end;

//��ֵ�ԣ���ʽ���ƣ�key1:value1,key2:value2
procedure TJsonPair.Parse(JsonString: String);
var
  List: TStringList;
  StrName: String;
begin
  List := TStringList.Create;
  try
    //�ԡ�:��Ϊ�ָ���������ǰ�ַ�����ͬ��ֻ�ǽ�����ǰ�� 
    Split(JsonString, ':', List);

    //��ֵ�Խ���������ֻ�������֣�����ֵ���������������˵����ʽ�����⣬����Ҫ�ڽ���ֱ�ӱ���
    if List.Count <> 2 then
    begin
      RaiseParseError(JsonString);
    end;
    
    StrName := List[0];
    //��ֵһ����һ���ַ���
    if not IsJsonString(StrName) then
    begin
      RaiseParseError(StrName);
    end;
    
    FName := Decode(Copy(StrName, 2, Length(StrName) - 2));

    //FValue: TJsonValue;   ������ֵ���н�����ֵ������һ���ַ�������ֵ������ṹ������ṹ��
    FValue.Parse(List[1]);
  finally
    List.Free;
  end;
end;

procedure TJsonPair.SetName(const Value: String);
begin
  FName := Value;
end;

//���ǵݹ����յĴ���߼�
//key:value
function TJsonPair.Stringify: String;
begin
  Result := Format('"%s":%s', [Encode(FName), FValue.Stringify]);
end;

{ TJsonObject }

function TJsonObject.Add(const Name: String): TJsonPair;
begin
  Result := TJsonPair.Create(Self, Name);
  FList.Add(Result);
end;

procedure TJsonObject.Assign(Source: TJsonBase);
var
  Src: TJsonObject;
  I: Integer;
begin
  Clear;
  if not(Source is TJsonObject) then
  begin
    RaiseAssignError(Source);
  end;

  //�ؼ��� as ���÷������ˣ�
  Src := Source as TJsonObject;
  
  for I := 0 to Src.Count - 1 do
  begin
    Add.Assign(Src.Items[I]);
  end;
end;

procedure TJsonObject.Clear;
var
  I: Integer;
  Item: TJsonPair;
begin
  for I := 0 to FList.Count - 1 do
  begin
    Item := FList[I];
    Item.Free;
  end;
  FList.Clear;
end;

constructor TJsonObject.Create(AOwner: TJsonBase);
begin
  inherited Create(AOwner);
  FList := TList.Create;
  FAutoAdd := True;
end;

procedure TJsonObject.Delete(const Index: Integer);
var
  Item: TJsonPair;
begin
  Item := FList[Index];
  Item.Free;
  FList.Delete(Index);
end;

procedure TJsonObject.Delete(const Name: String);
var
  Index: Integer;
begin
  Index := Find(Name);
  if Index < 0 then
  begin
    RaiseError(Format('"%s" not found', [Name]));
  end;
  Delete(Index);
end;

destructor TJsonObject.Destroy;
begin
  Clear;
  FList.Free;
  inherited Destroy;
end;

function TJsonObject.Find(const Name: String): Integer;
var
  I: Integer;
  Pair: TJsonPair;
begin
  Result := -1;
  for I := 0 to FList.Count - 1 do
  begin
    Pair := FList[I];
    if SameText(Name, Pair.Name) then
    begin
      Result := I;
      Break;
    end;
  end;
end;

function TJsonObject.GetCount: Integer;
begin
  Result := FList.Count;
end;

function TJsonObject.GetItems(Index: Integer): TJsonPair;
begin
  Result := FList[Index];
end;

function TJsonObject.GetValues(Name: String): TJsonValue;
var
  Index: Integer;
  Pair: TJsonPair;
begin
  Index := Find(Name);
  if Index < 0 then
  begin
    if not FAutoAdd then
    begin
      RaiseError(Format('%s not found', [Name]));
    end;
    Pair := Add(Name);
  end
  else Pair := FList[Index];
  Result := Pair.Value;
end;

function TJsonObject.Insert(const Index: Integer; const Name: String): TJsonPair;
begin
  Result := TJsonPair.Create(Self, Name);
  FList.Insert(Index, Result);
end;

procedure TJsonObject.Merge(Addition: TJsonObject);
var
  I: Integer;
begin
  for I := 0 to Addition.Count - 1 do Add.Assign(Addition.Items[I]);
end;

//��������ṹ��JsonString�ĸ�ʽ����ǣ�{key1:value1,key2:value2}
procedure TJsonObject.Parse(JsonString: String);
var
  I: Integer;
  S: String;
  List: TStringList;
  Item: TJsonPair;
begin
  Clear;
  JsonString := Trim(JsonString);
  if not IsJsonObject(JsonString) then
  begin
    RaiseParseError(JsonString);
  end;

  //�� {key1:value1,key2:value2} ת���� key1:value1,key2:value2
  S := Trim(Copy(JsonString, 2, Length(JsonString) - 2));

  List := TStringList.Create;
  try
    //Split�ǰ���','�ָǰ���Json��
    Split(S, ',', List);

    //ͬ���ģ��������������������������
    for I := 0 to List.Count - 1 do
    begin
      Item := Add;
      Item.Parse(List[I]);
    end;
  finally
    List.Free;
  end;
end;

function TJsonObject.Put(const Name: String; const Value: Integer): TJsonValue;
begin
  Result := Add(Name).Value;
  Result.AsInteger := Value;
end;

function TJsonObject.Put(const Name: String; const Value: Extended): TJsonValue;
begin
  Result := Add(Name).Value;
  Result.AsNumber := Value;
end;

function TJsonObject.Put(const Name: String; const Value: Boolean): TJsonValue;
begin
  Result := Add(Name).Value;
  Result.AsBoolean := Value;
end;

function TJsonObject.Put(const Name: String; const Value: TJsonEmpty): TJsonValue;
begin
  Result := Add(Name).Value;
  Result.IsEmpty := True;
end;

function TJsonObject.Put(const Name: String; const Value: TJsonNull): TJsonValue;
begin
  Result := Add(Name).Value;
  Result.IsNull := True;
end;

function TJsonObject.Put(const Name: String; const Value: TJsonValue): TJsonValue;
begin
  Result := Add(Name).Value;
  Result.Assign(Value);
end;

function TJsonObject.Put(const Value: TJsonPair): TJsonValue;
var
  Pair: TJsonPair;
begin
  Pair := Add;
  Pair.Assign(Value);
  Result := Pair.Value;
end;

function TJsonObject.Put(const Name: String; const Value: TJsonObject): TJsonValue;
begin
  Result := Add(Name).Value;
  Result.Assign(Value);
end;

function TJsonObject.Put(const Name, Value: String): TJsonValue;
begin
  Result := Add(Name).Value;
  Result.AsString := Value;
end;

function TJsonObject.Put(const Name: String; const Value: TJsonArray): TJsonValue;
begin
  Result := Add(Name).Value;
  Result.Assign(Value);
end;

//Json�Ķ���ṹ���
function TJsonObject.Stringify: String;
var
  I: Integer;
  Item: TJsonPair;
begin
  Result := '{';
  for I := 0 to FList.Count - 1 do
  begin
    Item := FList[I];
    if I > 0 then
    begin
      Result := Result + ',';
    end;

    //Json������ÿ��Ԫ�ض���һ����ֵ�ԣ�����ѭ������ÿ����ֵ��JsonPair��Stringify����
    Result := Result + Item.Stringify;
  end;
  Result := Result + '}';
end;

{ TJson }

procedure TJson.Assign(Source: TJsonBase);
begin
  Clear;
  //��������Delphi�Ĺؼ��� is ���ж�̬���ж�

  //���Source��TJson����
  if Source is TJson then                                                      
  begin
    case (Source as TJson).FStructType of
      jsNone: ;
      jsArray:
        begin
          CreateArrayIfNone;
          FJsonArray.Assign((Source as TJson).FJsonArray);
        end;                       
      jsObject:
        begin
          CreateObjectIfNone;
          FJsonObject.Assign((Source as TJson).FJsonObject);
        end;
    end;
  end
  //���Source��TJsonArray����
  else if Source is TJsonArray then
  begin
    CreateArrayIfNone;
    FJsonArray.Assign(Source);
  end
  //���Source��TJsonObject����
  else if Source is TJsonObject then
  begin
    CreateObjectIfNone;
    FJsonObject.Assign(Source);
  end
  //���Source��TJsonValue����
  else if Source is TJsonValue then
  begin
    //as �ؼ��ֽ� ����ָ������ �� ת��������ָ������
    //ǰ���Ǳ������ָ�����ָ���������
    if (Source as TJsonValue).ValueType = jvArray then
    begin
      CreateArrayIfNone;
      FJsonArray.Assign((Source as TJsonValue).AsArray);
    end
    else if (Source as TJsonValue).ValueType = jvObject then
    begin
      CreateObjectIfNone;
      FJsonObject.Assign((Source as TJsonValue).AsObject);
    end
    else
    begin
      RaiseAssignError(Source);
    end
  end
  else
  begin
    RaiseAssignError(Source);
  end;
end;

procedure TJson.CheckJsonArray;
begin
  CreateArrayIfNone;
  RaiseIfNotArray;
end;

procedure TJson.CheckJsonObject;
begin
  CreateObjectIfNone;
  RaiseIfNotObject;
end;

procedure TJson.Clear;
begin
  case FStructType of
    jsNone: ;
    jsArray:
      begin
        FJsonArray.Free;
        FJsonArray := nil;
      end;
    jsObject:
      begin
        FJsonObject.Free;
        FJsonObject := nil;
      end;
  end;
  FStructType := jsNone;
end;

constructor TJson.Create;
begin
  inherited Create(nil);
  FStructType := jsNone;
  FJsonArray := nil;
  FJsonObject := nil;
end;

procedure TJson.CreateArrayIfNone;
begin
  if FStructType = jsNone then
  begin
    FStructType := jsArray;
    FJsonArray := TJsonArray.Create(Self);
  end;
end;

procedure TJson.CreateObjectIfNone;
begin
  if FStructType = jsNone then
  begin
    FStructType := jsObject;
    FJsonObject := TJsonObject.Create(Self);
  end;
end;

procedure TJson.Delete(const Index: Integer);
begin
  RaiseIfNone;
  case FStructType of
    jsArray: FJsonArray.Delete(Index);
    jsObject: FJsonObject.Delete(Index);
  end;
end;

procedure TJson.Delete(const Name: String);
begin
  RaiseIfNotObject;
  FJsonObject.Delete(Name);
end;

destructor TJson.Destroy;
begin
  Clear;
  inherited Destroy;
end;

function TJson.Get(const Index: Integer): TJsonValue;
begin
  Result := nil;
  RaiseIfNone;
  case FStructType of
    jsArray: Result := FJsonArray.Items[Index];
    jsObject: Result := FJsonObject.Items[Index].Value;
  end;
end;

function TJson.Get(const Name: String): TJsonValue;
begin
  CheckJsonObject;
  Result := FJsonObject.Values[Name];
end;

function TJson.GetCount: Integer;
begin
  case FStructType of
    jsArray: Result := FJsonArray.Count;
    jsObject: Result := FJsonObject.Count;
  else
    Result := 0;
  end;
end;

function TJson.GetJsonArray: TJsonArray;
begin
  CheckJsonArray;
  Result := FJsonArray;
end;

function TJson.GetJsonObject: TJsonObject;
begin
  CheckJsonObject;
  Result := FJsonObject;
end;

function TJson.GetValues(Name: String): TJsonValue;
begin
  Result := Get(Name);
end;

//����Json��ʽ���ַ���
procedure TJson.Parse(JsonString: String);
begin
  Clear;
  JsonString := Trim(JsonString);

  //���JsonString��һ��Json����ṹ���ַ��������Ӧ����FJsonArray�Ľ�������
  //����JsonString��һ������ṹ���ַ������������ߵ�FJsonArray.Parse(JsonString);�ڲ���������ν�������ṹ���ַ�����
  if IsJsonArray(JsonString) then
  begin
    CreateArrayIfNone;
    FJsonArray.Parse(JsonString);
  end
  //���JsonString��һ��Json����ṹ���ַ��������Ӧ����FJsonObject�Ľ�������
  //����JsonString��һ������ṹ���ַ������������ߵ�FJsonObject.Parse(JsonString); �ڲ���������ν�������ṹ���ַ�����
  else if IsJsonObject(JsonString) then
  begin
    CreateObjectIfNone;
    FJsonObject.Parse(JsonString);
  end
  //��������������ָ�ʽ��ֱ���׳��쳣
  else
  begin
    RaiseParseError(JsonString);
  end;
end;

function TJson.Put(const Value: Integer): TJsonValue;
begin
  CheckJsonArray;
  Result := FJsonArray.Put(Value);
end;

function TJson.Put(const Value: Extended): TJsonValue;
begin
  CheckJsonArray;
  Result := FJsonArray.Put(Value);
end;

function TJson.Put(const Value: Boolean): TJsonValue;
begin
  CheckJsonArray;
  Result := FJsonArray.Put(Value);
end;

function TJson.Put(const Value: TJsonEmpty): TJsonValue;
begin
  CheckJsonArray;
  Result := FJsonArray.Put(Value);
end;

function TJson.Put(const Value: TJsonNull): TJsonValue;
begin
  CheckJsonArray;
  Result := FJsonArray.Put(Value);
end;

function TJson.Put(const Value: String): TJsonValue;
begin
  CheckJsonArray;
  Result := FJsonArray.Put(Value);
end;

function TJson.Put(const Value: TJsonValue): TJsonValue;
begin
  CheckJsonArray;
  Result := FJsonArray.Put(Value);
end;

function TJson.Put(const Value: TJsonObject): TJsonValue;
begin
  CheckJsonArray;
  Result := FJsonArray.Put(Value);
end;

function TJson.Put(const Value: TJsonArray): TJsonValue;
begin
  CheckJsonArray;
  Result := FJsonArray.Put(Value);
end;

function TJson.Put(const Name: String; const Value: Integer): TJsonValue;
begin
  CheckJsonObject;
  Result := FJsonObject.Put(Name, Value);
end;

function TJson.Put(const Name: String; const Value: Extended): TJsonValue;
begin
  CheckJsonObject;
  Result := FJsonObject.Put(Name, Value);
end;

function TJson.Put(const Name: String; const Value: Boolean): TJsonValue;
begin
  CheckJsonObject;
  Result := FJsonObject.Put(Name, Value);
end;

function TJson.Put(const Name: String; const Value: TJsonEmpty): TJsonValue;
begin
  CheckJsonObject;
  Result := FJsonObject.Put(Name, Value);
end;

function TJson.Put(const Name: String; const Value: TJsonNull): TJsonValue;
begin
  CheckJsonObject;
  Result := FJsonObject.Put(Name, Value);
end;

function TJson.Put(const Name: String; const Value: TJsonValue): TJsonValue;
begin
  CheckJsonObject;
  Result := FJsonObject.Put(Name, Value);
end;

function TJson.Put(const Value: TJsonPair): TJsonValue;
begin
  CheckJsonObject;
  Result := FJsonObject.Put(Value);
end;

function TJson.Put(const Name: String; const Value: TJsonObject): TJsonValue;
begin
  CheckJsonObject;
  Result := FJsonObject.Put(Name, Value);
end;

function TJson.Put(const Name, Value: String): TJsonValue;
begin
  CheckJsonObject;
  Result := FJsonObject.Put(Name, Value);
end;

function TJson.Put(const Name: String; const Value: TJsonArray): TJsonValue;
begin
  CheckJsonObject;
  Result := FJsonObject.Put(Name, Value);
end;

function TJson.Put(const Value: TJson): TJsonValue;
begin
  CheckJsonArray;
  case Value.FStructType of
    jsArray: Result := Put(Value.FJsonArray);
    jsObject: Result := Put(Value.FJsonObject);
  else
    Result := nil;
  end;
end;

function TJson.Put(const Name: String; const Value: TJson): TJsonValue;
begin
  CheckJsonObject;
  case Value.FStructType of
    jsArray: Result := Put(Name, Value.FJsonArray);
    jsObject: Result := Put(Name, Value.FJsonObject);
  else
    Result := nil;
  end;
end;

procedure TJson.RaiseIfNone;
begin
  if FStructType = jsNone then
  begin
    RaiseError('json struct type is jsNone');
  end;
end;

procedure TJson.RaiseIfNotArray;
begin
  if FStructType <> jsArray then
  begin
    RaiseError('json struct type is not jsArray');
  end;
end;

procedure TJson.RaiseIfNotObject;
begin
  if FStructType <> jsObject then
  begin
    RaiseError('json struct type is not jsObject');
  end;
end;

//��Json�ĸ���Ԫ�ؽڵ����õ�һ��Json��ʽ���ַ���
function TJson.Stringify: String;
begin
  case FStructType of
    jsArray:
      Result := FJsonArray.Stringify;
    jsObject:
      Result := FJsonObject.Stringify;
  else
    Result := '';
  end;
end;

end.
