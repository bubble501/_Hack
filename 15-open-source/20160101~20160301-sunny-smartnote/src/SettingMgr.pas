////////////////////////////////////////////////////////////////////////////////
//
//
//  FileName    :   SettingMgr.pas
//  Creator     :   Shen Min
//  Date        :   2002-4-29
//  Comment     :
//
//
////////////////////////////////////////////////////////////////////////////////

unit SettingMgr;

{每个应用软件都会有功能上的选项允许用户设置，而这些设置可能会涉及各个方面，
  如何在程序中有序地对这些选项设置进行管理是非常重要的
  可以将默认的选项设置的管理逻辑抽象出来，形成一个独立的类，由它负责集中管理，其职责包括
    1.箱其他模块提供某一个选项的默认设置值
    2.接受新的某选项默认设置值
    3.在应用程序启动时，从外部（磁盘或注册表）取得所有选项的默认设置值
    4.在应用程序退出时保存所有设置的值到外部（磁盘或注册表）
  }
{这个抽象成为TssnSettingMgr
  首先考虑算法，在程序启动时，将所有选项的默认值从外部（磁盘）读取到内存中。
    其他模块请求获取某个值时，将内存中的该值返回给它；
    其他模块请求更新某个值时，更新内存中的数据
    在需要的时候（程序退出时或者更新过某个值时）将所有设置的值保存到外部（磁盘或注册表）
  然后考虑实现，只需为每一个选项提供一个内部的数据成员保存其值，并提供这些值的访问接口
    在该类被初始化时（程序启动时），从外部（磁盘或注册表）取得默认设置值来初始化这些数据成员
    在该类被析构时（程序退出时），将这些数据成员的值保存到外部（磁盘或注册表）
    另外，客户程序在设置某一个选项的默认值是，可能要求立刻保存到外部（磁盘或注册表）以增加安全性
    TssnSettingMgr也应该满足这个需求
  }
{在Sunny SmartNote的开放源码版本这种，为简化起见，选项设置模块只允许设置工作区的默认字体}

interface

uses Graphics, IniFiles;

type
    TssnSettingMgr = class
    private
        m_SaveWhenSet : Boolean;    //是否在设置选项默认值时，立即保存到外部（磁盘或注册表）

    protected
        {设置的名称，其意义依赖不同实现而不同
          例如，如果保存设置的是磁盘文件，则该名称可被作为文件名
          如果保存设置到注册表，则该名称可被用作键名
          }
        m_SettingName : String;

        m_Font : TFont;   //保存默认字体的数据成员

        {如果有其他选项，可加入更多数据成员}


        {下面的两个方法是virtual的，意味着在TssnSettingMgr中姜维它们提供一种默认的实现——从磁盘文件中存取
          当然，TssnSettingMgr的派生类可以override这两个函数，以提供不同的存取方式，如保存到系统注册表}
        procedure LoadSettings(); virtual;  //从外部（磁盘或注册表）读取所有默认设置值
        procedure SaveSettings(); virtual;  //保存所有默认设置值到外部（磁盘或注册表）

    public
        procedure SetDefaultFont(Font : TFont);   //设置默认字体的方法
        function GetDefaultFont() : TFont;        //取得默认字体的方法

        {如果有其他选项，可加入更多的方法}

        constructor Create(SettingName : String; SaveWhenSet : Boolean);
        destructor Destroy(); override;
    end;

implementation

uses ssnPublic;

{ TssnSettingMgr }

constructor TssnSettingMgr.Create(SettingName: String; SaveWhenSet : Boolean);
begin
    m_SettingName := SettingName;
    m_SaveWhenSet := SaveWhenSet;

    m_Font := TFont.Create();;

    LoadSettings();    //读取所有设置
end;

destructor TssnSettingMgr.Destroy;
begin
    SaveSettings();    //保存所有设置

    m_Font.Free();
end;

function TssnSettingMgr.GetDefaultFont: TFont;
begin
    Result := m_Font;
end;

procedure TssnSettingMgr.LoadSettings;
var
    IniFile : TIniFile;
begin
    IniFile := TIniFile.Create(GetExePath() + m_SettingName + '.ini');

    //字体的选项默认值读取
    m_Font.Name := IniFile.ReadString('Editor', 'FontName', 'Comic Sans MS');
    m_Font.Size := IniFile.ReadInteger('Editor', 'FontSize', 10);
    m_Font.Color := IniFile.ReadInteger('Editor', 'FontColor', clBlack);

    {如果有其他选项的话，可以在此加入}

    IniFile.Free();
end;

procedure TssnSettingMgr.SaveSettings;
var
    IniFile : TIniFile;
begin
    IniFile := TIniFile.Create(GetExePath() + m_SettingName + '.ini');

    // 字体的选项默认值保存
    IniFile.WriteString('Editor', 'FontName', m_Font.Name);
    IniFile.WriteInteger('Editor', 'FontSize', m_Font.Size);
    IniFile.WriteInteger('Editor', 'FontColor', m_Font.Color);

    {如果有其他的选项的话，可在此加入}

    IniFile.Free();
end;

procedure TssnSettingMgr.SetDefaultFont(Font: TFont);
begin
    m_Font.Assign(Font);

    if m_SaveWhenSet then   //如果需要立即保存
        SaveSettings();
end;

end.
