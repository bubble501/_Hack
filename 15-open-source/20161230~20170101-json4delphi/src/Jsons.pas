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
  //每个Json键值对中值的类型：None、Null、字符串、数值、布尔、对象、数组
  TJsonValueType = (jvNone, jvNull, jvString, jvNumber, jvBoolean, jvObject, jvArray);
  //
  TJsonStructType = (jsNone, jsArray, jsObject);
  //
  TJsonNull = (null);
  //
  TJsonEmpty = (empty);

type
  //TJsonBase是本Json解析单元中所有类的基类
  TJsonBase = class(TObject)
  private
    {Json是多层嵌套的格式，总的来说类似于一个树形结构
      数据结构中学习的二叉树时，定义每个节点是：值、左节点、右节点
      但是Json对应的树并不是二叉树，而是每个节点可能有0、1、2、3…n个子节点
      其实很多事情反向思考能看到不一样的世界：不从上往下看，而是从下往上看
      可以抽象得到：除了根节点外，其他每个节点都有一个父节点，根节点没有父节点
      本单元中的Json解析就是通过反向构建树来设计类去解析和打包Json的}
    FOwner: TJsonBase;                     //它的父元素信息
    function GetOwner: TJsonBase;          //获取它的父元素

  protected
    function GetOwnerName: String;         //获取父元素的名称
    //异常相关方法
    procedure RaiseError(const Msg: String);
    procedure RaiseParseError(const JsonString: String);
    procedure RaiseAssignError(Source: TJsonBase);
    
  public
    constructor Create(AOwner: TJsonBase);  //根据父元素构建一个对象
    destructor Destroy; override;           //释放对象

    //规定解析接口，基类并不实现，只约定接口，具体由子类实现
    procedure Parse(JsonString: String); virtual; abstract;

    //规定打包接口，基类并不实现，只约定接口，具体由子类实现
    function Stringify: String; virtual; abstract;

    //Assign方法主要应用于Put方法中TJsonBase子类对象之间的赋值操作
    //因为Number、string、Boolean类型可以直接使用 := 进行赋值
    //TJsonObject、TJsonValue、TJsonArray不能直接使用 := 传值，因为Delphi通过类定义的对象其实只是一个指针，:= 也只是指针指向的地址变化，会有问题
    //所以以下的Put方法中需要调用各个TBase子类的Assign方法赋值
    //Put(const Value: TJsonObject)、Put(const Value: TJsonValue)、Put(const Value: TJsonArray)
    procedure Assign(Source: TJsonBase); virtual; abstract;

    function Encode(const S: String): String;
    function Decode(const S: String): String;

    //按照分隔符将Json包按照当前的层进行分隔成多份，因为Json是多层包结构，所以按照当前层分隔是很重要的！
    //Split方法是解析Json结构字符串的很核心的方法
    procedure Split(const S: String; const Delimiter: Char; Strings: TStrings);

    //判断值是不是某种结构
    function IsJsonObject(const S: String): Boolean;         //判断值是不是一个Json对象结构
    function IsJsonArray(const S: String): Boolean;          //判断值是不是一个Json数组结构
    function IsJsonString(const S: String): Boolean;         //判断值是不是一个Json字符串
    function IsJsonNumber(const S: String): Boolean;         //判断值是不是一个Json数值
    function IsJsonBoolean(const S: String): Boolean;        //判断值是不是一个Json布尔
    function IsJsonNull(const S: String): Boolean;           //判断值是不是一个Json Null
    //判断值具体是什么结构
    function AnalyzeJsonValueType(const S: String): TJsonValueType;

  public
    property Owner: TJsonBase read GetOwner;
    
  end;

  //Json对象结构对应的类
  //Json对象结构，以{开始，以}结尾
  TJsonObject = class;

  //Json数组结构对应的类
  //Json数组结构，以[开始，以]结尾
  TJsonArray = class;

  //Json键值对结构的值对应的类
  //值可以是对象结构、可以是数组结构、可以是字符串、可以是数值、可以是布尔、……
  //【注意】TJsonPair才是一个Json键值对结构的类，而TJsonValue只是键值对中值对应的类
  TJsonValue = class(TJsonBase)
  private
    FValueType: TJsonValueType;     //每个Json键值对中值的类型：None、Null、字符串、数值、布尔、对象、数组
    FStringValue: String;           //如果键值对的值是字符串类型，那么对应的值的内容
    FNumberValue: Extended;         //如果键值对的值是数值类型，那么对应的值的内容
    FBooleanValue: Boolean;         //如果键值对的值是布尔类型，那么对应的值的内容
    FObjectValue: TJsonObject;      //如果键值对的值是对象结构，那么对应的值的内容
    FArrayValue: TJsonArray;        //如果键值对的值是数组结构，那么对应的值的内容

    function GetAsBoolean: Boolean;     //获取键值对的值，以布尔类型获取值
    function GetAsInteger: Integer;     //获取键值对的值，以整型获取值
    function GetAsNumber: Extended;     //获取键值对的值，以数值型获取
    function GetAsArray: TJsonArray;    //获取键值对的值，以数组结构获取值
    function GetAsObject: TJsonObject;  //获取键值对的值，以对象结构获取
    function GetAsString: String;       //获取键值对的值，以字符串类型获取
    function GetIsNull: Boolean;        //获取键值对的值，判断其是不是NULL
    function GetIsEmpty: Boolean;       //获取键值对的值，判断其实是不是Empty
    
    procedure SetAsBoolean(const Value: Boolean);      //设置键值对的值，布尔类型
    procedure SetAsInteger(const Value: Integer);      //设置键值对的值，整型
    procedure SetAsNumber(const Value: Extended);      //设置键值对的值，数值
    procedure SetAsString(const Value: String);        //设置键值对的值，字符串
    procedure SetAsArray(const Value: TJsonArray);     //设置键值对的值，数组结构
    procedure SetAsObject(const Value: TJsonObject);   //设置键值对的值，对象结构
    procedure SetIsNull(const Value: Boolean);         //设置键值对的值，是不是NULL
    procedure SetIsEmpty(const Value: Boolean);        //设置键值对的值，是不是Empty

  protected
    procedure RaiseValueTypeError(const AsValueType: TJsonValueType);

  public
    constructor Create(AOwner: TJsonBase);
    destructor Destroy; override;

    //实现父类定义的解析方法，解析对应的键值对字符串
    //解析一个键值对的值部分，根据其类型是数值 or 字符串 or 数组 or 对象 …，而得到对应的数值值 or 字符串值 or 数组值 or 对象值 …
    procedure Parse(JsonString: String); override;

    //实现父类定义的打包方法，打包得到对应的键值对字符串
    //根据设置的值是数值 or 字符串 or 数组 or 对象 …，对应打包得到数值型 or 字符串型 or 数值型 or 对象型 …的字符串包
    function Stringify: String; override;

    //实现父类定义的赋值方法，Source对应可以是TJsonValue、TJsonArray、TJsonObject
    procedure Assign(Source: TJsonBase); override;

    procedure Clear;

  public
    //对应的Set、Get
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

  //Json数组结构对应的类
  //Json数组结构，以[开始，以]结尾
  //对于Json格式，数组内部包含的可以是字符串、数值、布尔、数组、对象类型的值
  //在本解包打包类的实现中，Json数组的索引是从0而不是从1开始的
  TJsonArray = class(TJsonBase)
  private
    //使用链表存储数组结构中每个元素，元素可以是TJsonArray、TJsonObject、TJsonValue
    //上面说到Json格式其实就是一个树形数据结构，在TJsonBase中使用FOwner: TJsonBase;来保存每个节点的父节点
    //其实还使用链表来保存了每个节点的子节点！
    //不过只有TJsonArray和TJsonObject会有子节点，需要使用List来管理子节点
    FList: TList;
    function GetItems(Index: Integer): TJsonValue;   //获取数组结构的第Index个元素，每个元素时TJsonValue类型
    function GetCount: Integer;                      //获取数组结构中元素的个数
    
  public
    constructor Create(AOwner: TJsonBase = nil);
    destructor Destroy; override;

    //实现父类定义的解析方法，解析对应的数组结构
    //这个Parse方法的JsonString参数对应的是类似 [{key1:value1,key2:value2},{key3:value3}] 结构的字符串
    procedure Parse(JsonString: String); override;

    //实现父类定义的打包方法，打包得到对应的数组结构
    //根据该数据结构中的元素，打包得到类似 [{key1:value1,key2:value2},{key3:value3}] 结构的字符串
    function Stringify: String; override;

    //实现父类定义的赋值方法，Source只能是TJsonArray类型
    procedure Assign(Source: TJsonBase); override;

    //合并两个数组结构到一个数组结构中
    procedure Merge(Addition: TJsonArray);

    function Add: TJsonValue;                                     //往数组结构中再加一个元素
    function Insert(const Index: Integer): TJsonValue;            //

    //往数组中添加对应类型的元素，内部通过调用Add方法实现
    function Put(const Value: TJsonEmpty): TJsonValue; overload;  //添加一个Empty类型的元素
    function Put(const Value: TJsonNull): TJsonValue; overload;   //添加一个Null类型的元素
    function Put(const Value: Boolean): TJsonValue; overload;     //添加一个布尔类型的元素
    function Put(const Value: Integer): TJsonValue; overload;     //添加一个整型的元素
    function Put(const Value: Extended): TJsonValue; overload;    //添加一个数值型的元素
    function Put(const Value: String): TJsonValue; overload;      //添加一个字符串型的元素
    function Put(const Value: TJsonArray): TJsonValue; overload;  //添加一个数组结构的元素
    function Put(const Value: TJsonObject): TJsonValue; overload; //添加一个对象结构的元素
    function Put(const Value: TJsonValue): TJsonValue; overload;  //添加一个键值对的元素

    procedure Delete(const Index: Integer);                       //删除第Index个元素
    procedure Clear;
    
  public
    property Count: Integer read GetCount;                             //获取数组中元素的个数
    property Items[Index: Integer]: TJsonValue read GetItems; default; //根据索引获取第Index个元素              
  end;

  //Json键值对结构对应的类
  //键是string类型，值是TJsonValue类型
  //对于Json格式，值可以是字符串类型、数值型、布尔型、对象结构、数组结构
  TJsonPair = class(TJsonBase)
  private
    FName: String;        //一个键值对的键
    FValue: TJsonValue;   //一个键值对的值

    procedure SetName(const Value: String);   //设置一个键值对的键
    
  public
    constructor Create(AOwner: TJsonBase; const AName: String = '');
    destructor Destroy; override;

    //这个Parse方法的参数是类似 key:value 结构的字符串，该方法解析该字符串获取对应的键和值
    procedure Parse(JsonString: String); override;

    //根据键和值，打包得到类似 key:value 结构的字符串
    function Stringify: String; override;

    //实现父类定义的赋值方法，Source只能是TJsonPair类型
    procedure Assign(Source: TJsonBase); override;
    
  public
    property Name: String read FName write SetName;
    property Value: TJsonValue read FValue;
    
  end;

  //Json对象结构对应的类
  //Json对象结构，以{开始，以}结尾
  //对于Json格式，对象结构内部由多个键值对组成
  TJsonObject = class(TJsonBase)
  private
    //使用链表存储数组结构中每个元素，元素可以是TJsonArray、TJsonObject、TJsonValue
    //上面说到Json格式其实就是一个树形数据结构，在TJsonBase中使用FOwner: TJsonBase;来保存每个节点的父节点
    //其实还使用链表来保存了每个节点的子节点！
    //不过只有TJsonArray和TJsonObject会有子节点，需要使用List来管理子节点
    FList: TList;
    FAutoAdd: Boolean;
    function GetCount: Integer;
    function GetItems(Index: Integer): TJsonPair;
    function GetValues(Name: String): TJsonValue;
    
  public
    constructor Create(AOwner: TJsonBase = nil);
    destructor Destroy; override;

    //实现父类定义的解析方法，解析对应的对象结构
    //这个Parse方法的JsonString参数对应的是类似 {key1:value1,key2:value2} 结构的字符串
    procedure Parse(JsonString: String); override;

    //实现父类定义的打包方法，打包得到对应的对象结构
    //根据该数据结构中的元素，打包得到类似 {key1:value1,key2:value2} 结构的字符串
    function Stringify: String; override;

    //实现父类定义的赋值方法，Source只能是TJsonObject类型
    procedure Assign(Source: TJsonBase); override;

    //合并两个对象结构为一个对象结构
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
    property Items[Index: Integer]: TJsonPair read GetItems;            //用法 XXX[i].
    property Values[Name: String]: TJsonValue read GetValues; default;  //用法 XXX['S'].
    property AutoAdd: Boolean read FAutoAdd write FAutoAdd;
    
  end;

  //具体在打包得到一个完整的Json字符串、解包一个完整的Json字符串时，要使用TJson类
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

    //解析一个完整的Json字符串，这个Parse的参数JsonString是一个完整的Json字符串
    procedure Parse(JsonString: String); override;

    //打包得到一个完整的Json字符串
    function Stringify: String; override;

    //实现父类定义的赋值方法，Source可以是以下类型：TJson、TJsonArray、TJsonObject、TJsonValue
    procedure Assign(Source: TJsonBase); override;

    procedure Delete(const Index: Integer); overload;
    procedure Delete(const Name: String); overload;

    procedure Clear;

    function Get(const Index: Integer): TJsonValue; overload;   //for both
    function Get(const Name: String): TJsonValue; overload;     //for JsonObject

    //供打包数组结构使用
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

    //供打包普通的键值对使用
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

//判断值到底是什么结构
function TJsonBase.AnalyzeJsonValueType(const S: String): TJsonValueType;
var
  Len: Integer;
  Number: Extended;
begin
  Result := jvNone;
  Len := Length(S);
  //对象、数组、字符串、null、布尔，长度必然都大于等于2
  if Len >= 2 then
  begin
    //如果是{开头、}结尾，那么是对象结构
    if (S[1] = '{') and (S[Len] = '}') then
    begin
      Result := jvObject
    end
    //如果是[开头、]结尾，那么是数组结构
    else if (S[1] = '[') and (S[Len] = ']') then
    begin
      Result := jvArray
    end
    //如果是"开头、"结尾，那么是字符串
    else if (S[1] = '"') and (S[Len] = '"') then
    begin
      Result := jvString
    end
    //如果值是null，那么是null
    else if SameText(S, 'null') then
    begin
      Result := jvNull
    end
    //如果值是true或false，那么是布尔
    else if SameText(S, 'true') or SameText(S, 'false') then
    begin
      Result := jvBoolean
    end
    //如果值可以转换成Float，那么是数值
    else if TryStrToFloat(S, Number) then
    begin
      Result := jvNumber;
    end;
  end
  //数值长度则不一定大于等于2
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

//判断值是不是一个 Json对象结构
//Json对象的特点是：以“{”开头，以“}”结尾
function TJsonBase.IsJsonObject(const S: String): Boolean;
var
  Len: Integer;
begin
  Len := Length(S);
  Result := (Len >= 2) and (S[1] = '{') and (S[Len] = '}');
end;

//判断值是不是一个 Json数组结构
//Json数组的特点是：以“[”开头，以“]”结尾
function TJsonBase.IsJsonArray(const S: String): Boolean;
var
  Len: Integer;
begin
  Len := Length(S);
  Result := (Len >= 2) and (S[1] = '[') and (S[Len] = ']');
end;

//判断值是不是 Json布尔
//Json布尔的特点是值是true或false
function TJsonBase.IsJsonBoolean(const S: String): Boolean;
begin
  Result := SameText(S, 'true') or SameText(S, 'false');
end;

//判断值是不是 Json Null
//Json Null的特点是值是null
function TJsonBase.IsJsonNull(const S: String): Boolean;
begin
  Result := SameText(S, 'null');
end;

//判断值是不是 Json数值
//Json数值的特点是可以转换成Float类型
function TJsonBase.IsJsonNumber(const S: String): Boolean;
var
  Number: Extended;
begin
  Result := TryStrToFloat(S, Number);
end;

//判断值是不是一个Json字符串
//Json字符串的特点是：以“"”开头，以“"”结尾
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

//解析{key1:value1,key2:value2},{key3:value3} 得到 {key1:value1,key2:value2} 和 {key3:value3}
//  而不是{key1:value1 和 key2:value2} 和 {key3:value3}
//  Splite方法中会根据“{”去匹配“}”，然后判断只解析第一层
procedure TJsonBase.Split(const S: String; const Delimiter: Char; Strings: TStrings);

  function IsPairBegin(C: Char): Boolean;
  begin
    Result := (C = '{') or (C = '[') or (C = '"');
  end;

  //获取字符的另一对应的字符
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

  //假如当前指针P 指向'{'，那么通过调用该方法找到'{'对应的'}'所在的地址，作为返回值返回！
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
  //假设S='{key1:value1,key2:value2},{key3:value3}'
  PtrBegin := PChar(S);
  PtrEnd := PtrBegin;

  //轮询字符串数组
  while PtrEnd^ <> #0 do
  begin
    C := PtrEnd^;

    //第一次C='{'进入else if ，在else if中调用 MoveToPair，将指针调到和'{'一组对应的'}'处，然后继续解析
    //这样就可以按照','分解Json字符串时只分解当前的最外层，而不受内层的','影响！
    if C = Delimiter then
    begin
      StrItem := Trim(Copy(PtrBegin, 1, PtrEnd - PtrBegin));
      Strings.Add(StrItem);
      PtrBegin := PtrEnd + 1;
      PtrEnd := PtrBegin;
      Continue;
    end
    //如果不是分隔符，而是{、[或"
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

//Json包结构中键值对的值的打包方法
function TJsonValue.Stringify: String;
const
  StrBoolean: array[Boolean] of String = ('false', 'true');
begin
  Result := '';
  case FValueType of
    //none或null，直接给值null
    jvNone, jvNull:
      Result := 'null';
    //字符串类型，直接在字符串值前后加 "
    jvString:
      Result := '"' + Encode(FStringValue) + '"';
    //数值类型，直接将数值转换成字符串格式
    jvNumber:
      Result := FloatToStr(FNumberValue);
    //布尔类型
    jvBoolean:
      Result := StrBoolean[FBooleanValue];
    //值还可以是Json对象
    jvObject:
      Result := FObjectValue.Stringify;
    //值还可以是Json数组
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

//解析类似 [{key1:value1,key2:value2},{key3:value3},123] 结构的字符串
procedure TJsonArray.Parse(JsonString: String);
var
  I: Integer;
  S: String;
  List: TStringList;
  Item: TJsonValue;
begin
  Clear;
  JsonString := Trim(JsonString);

  //先判断，如果JsonString不是数组结构的字符串则直接抛出异常结束解析
  if not IsJsonArray(JsonString) then
  begin
    RaiseParseError(JsonString);
  end;

  //将类似 [{key1:value1,key2:value2},{key3:value3}] 的字符串转换成 {key1:value1,key2:value2},{key3:value3}
  S := Trim(Copy(JsonString, 2, Length(JsonString) - 2));

  //使用
  List := TStringList.Create;
  try
    //Split方法同样是在该单元中实现的
    //比如其解析{key1:value1,key2:value2},{key3:value3}
    //只是对第一层进行解析，得到{key1:value1,key2:value2} 和 {key3:value3}
    //而不是{key1:value1 和 key2:value2} 和 {key3:value3}
    //有必要去看一下这个方法是如何在多层结构的情况下进行解析的
    //这个方法是本单元源码中一个比较核心的方法
    Split(S, ',', List);

    //将该层的所有值都解析出来后，然后对所有值再逐步往最底层进行解析
    //类似于对树进行深度优先搜索
    //Json就是多层的树形结构，而本单元Json解析的实现中对其的解析是先沿着一个分支解析到最后一层，然后再解析另一个分支！
    //也就是Parse内部在调用下一层的Parse，然后再调用下一层的Parse，直到调用到最后一层，然后再去解析下一个分支……
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

//对一个数组结构进行打包
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

    //数组内部的元素都是键值对的值，所以分别调用TJsonValue的打包方法
    //TJsonArray中使用链表FList存储其各个值元素
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

//键值对，格式类似：key1:value1,key2:value2
procedure TJsonPair.Parse(JsonString: String);
var
  List: TStringList;
  StrName: String;
begin
  List := TStringList.Create;
  try
    //以“:”为分隔符解析当前字符串，同样只是解析当前层 
    Split(JsonString, ':', List);

    //键值对解析出来后，只有两部分，键和值，如果不是两个，说明格式有问题，不需要在解析直接报错！
    if List.Count <> 2 then
    begin
      RaiseParseError(JsonString);
    end;
    
    StrName := List[0];
    //键值一定是一个字符串
    if not IsJsonString(StrName) then
    begin
      RaiseParseError(StrName);
    end;
    
    FName := Decode(Copy(StrName, 2, Length(StrName) - 2));

    //FValue: TJsonValue;   继续对值进行解析，值可能是一个字符串、数值、对象结构、数组结构…
    FValue.Parse(List[1]);
  finally
    List.Free;
  end;
end;

procedure TJsonPair.SetName(const Value: String);
begin
  FName := Value;
end;

//这是递归最终的打包逻辑
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

  //关键字 as 的用法大概如此！
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

//解析对象结构，JsonString的格式大概是：{key1:value1,key2:value2}
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

  //将 {key1:value1,key2:value2} 转换成 key1:value1,key2:value2
  S := Trim(Copy(JsonString, 2, Length(JsonString) - 2));

  List := TStringList.Create;
  try
    //Split是按照','分割当前层的Json包
    Split(S, ',', List);

    //同样的，解析类似于树的深度优先搜索
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

//Json的对象结构打包
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

    //Json对象中每个元素都是一个键值对，所以循环调用每个键值对JsonPair的Stringify方法
    Result := Result + Item.Stringify;
  end;
  Result := Result + '}';
end;

{ TJson }

procedure TJson.Assign(Source: TJsonBase);
begin
  Clear;
  //下面利用Delphi的关键字 is 进行多态的判断

  //如果Source是TJson类型
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
  //如果Source是TJsonArray类型
  else if Source is TJsonArray then
  begin
    CreateArrayIfNone;
    FJsonArray.Assign(Source);
  end
  //如果Source是TJsonObject类型
  else if Source is TJsonObject then
  begin
    CreateObjectIfNone;
    FJsonObject.Assign(Source);
  end
  //如果Source是TJsonValue类型
  else if Source is TJsonValue then
  begin
    //as 关键字将 父类指针类型 再 转换成子类指针类型
    //前提是本身这个指针就是指向子类对象！
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

//解析Json格式的字符串
procedure TJson.Parse(JsonString: String);
begin
  Clear;
  JsonString := Trim(JsonString);

  //如果JsonString是一个Json数组结构的字符串，则对应调用FJsonArray的解析方法
  //假设JsonString是一个数组结构的字符串，那我们走到FJsonArray.Parse(JsonString);内部看其是如何解析数组结构的字符串的
  if IsJsonArray(JsonString) then
  begin
    CreateArrayIfNone;
    FJsonArray.Parse(JsonString);
  end
  //如果JsonString是一个Json对象结构的字符串，则对应调用FJsonObject的解析方法
  //假设JsonString是一个对象结构的字符串，那我们走到FJsonObject.Parse(JsonString); 内部看其是如何解析对象结构的字符串的
  else if IsJsonObject(JsonString) then
  begin
    CreateObjectIfNone;
    FJsonObject.Parse(JsonString);
  end
  //如果不是以上两种格式则直接抛出异常
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

//将Json的各层元素节点打包得到一个Json格式的字符串
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
