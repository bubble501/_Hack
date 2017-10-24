////////////////////////////////////////////////////////////////////////////////
//
//
//  FileName    :   TabWorkSpaceMgr.pas
//  Creator     :   Shen Min
//  Date        :   2002-4-2
//  Comment     :
//
//
////////////////////////////////////////////////////////////////////////////////

unit TabWorkSpaceMgr;

interface

uses comctrls, Classes, ExtCtrls, Controls, Sysutils,
     WorkSpaceMgr, WorkSpace, MultiLan;

{上面接连实现了TssnWorkSpace和TssnWorkSpaceMgr，它们都是抽象类，并不能直接创建出它们的实例，
  而是需要等待它们的派生类来合作，具体实现某一多工作区的界面风格

  由于TssnTabWorkSpaceMgr和TssnTabWorkSpace在实现上的关联性，所以将它们两个的实现放在一个单元文件里面
    所以怎么分配各个类在哪个单元文件里面都是有讲究的
    这样可以分类清晰、方便管理
  }

{目前我们暂时使用分页的用户界面实现多工作区
  因此，根据多页的组件（TPageControl）要求，我们从TssnWorkSpace派生实现了TssnTabWorkSpace
  从TssnWorkSpaceMgr派生实现了TssnTabWorkSpaceMgr

当然，需求也要求可以随时更换多工作区的实现，那么只需要再从TssnWorkSpace和TssnWorkSpaceMgr派生出一套新的界面风格实现即可
  }

type
    {TssnTabWorkSpaceMgr派生自TssnWorkSpaceMgr，其职责是用TPageControl组件实现一个可实例化的工作区管理器
      因此TssnTabWorkSpaceMgr只需要内部包含一个TPageControl的实例，并实现TssnWorkSpaceMgr声明的抽象虚方法即可
      }
    TssnTabWorkSpaceMgr = class(TssnWorkSpaceMgr)
    private
        m_Tab : TPageControl;

    protected
        {配合基类的NewWorkSpace()完成Template Method模式的DoNewWorkSpace
          }
        function DoNewWorkSpace(var WorkSpace : TssnWorkSpace; FileName : String; nIndex : Integer) : Integer; override;

        {配合ActiveWorkSpace的DoActiveWorkSpace
          }
        procedure DoActiveWorkSpace(nIndex : Integer); override;

        {获取当前激活工作区的标识符（索引值）
          它只是把在每个TTabSheet组件的tag属性中的值取出来而已
          }
        function GetActiveWorkSpaceIndex() : Integer; override;

    public
        {其构造函数有一个ParentCtrl参数，以指定TPageControl组件放置在哪一个组件里
          在构造函数中完成对TPageControl实例--m_Tab的创建和初始化工作
          }
        constructor Create(ParentCtrl : TWinControl);
        destructor Destroy(); override;

    end;

    {TssnTabWorkSpace派生自TssnWorkSpace，在逻辑上的职责是具体实现一个工作区的风格
      多工作区界面风格已经确定使用分页组件TPageControl来实现，TPageControl组件其实是分页中的每一页--TTabSheet的容器
      每创建一个页，也就是创建了一个TTabSheet的实例
      TPageControl和TTabSheet的关系，与TssnWorkSpaceMgr和TssnWorkSpace的关系是多么类似呀
      很显然在实现上，TssnTabWorkSpace（工作区）对应于TTabSheet，TssnTabWorkSpaceMgr（工作区管理器）对应于TPageControl
      }
    TssnTabWorkSpace = class(TssnWorkSpace)
    private
        m_TabSheet : TTabSheet;
        procedure SetCaption(FileName : String);  //为每个TTabSheet的实例设置一个标题

    protected
        procedure OnSave(); override;

    public
        {Mgr指定管理器的组件，是一个TPageControl
         FileName给出工作区需要打开的文件名，无标题工作区则指定文件名为''
         nIndex是工作区管理器分配给新工作区的标识符}
        constructor Create(Mgr : TPageControl; FileName : String; nIndex : Integer);
        destructor Destroy(); override;

        {获取PageControl的索引值
          该索引值与“工作区”编号索引值不同

          需要向其管理器与实现相关者--TssnTabWorkSpaceMgr提供每个TTabSheet实例在TPageControl中的索引值
            该索引值与为每个工作区赋予的标识符不同，工作区的标识符其实是一个逻辑上的符号（编号）
            它与TTabSheet在TPageControl中的索引值是不同的
            该索引值是TPageControl在管理多个TTabSheet时所使用
          }
        function TabWS_GetPageIndex() : Integer;
    end;

implementation

uses GlobalObject;

{ TssnTabWorkSpaceMgr }
{其构造函数有一个ParentCtrl参数，以指定TPageControl组件放置在哪一个组件里
  在构造函数中完成对TPageControl实例--m_Tab的创建和初始化工作
  }
constructor TssnTabWorkSpaceMgr.Create(ParentCtrl: TWinControl);
begin
    inherited Create();

    m_Tab := TPageControl.Create(ParentCtrl);
    m_Tab.Parent := ParentCtrl;
    m_Tab.Align := alClient;
    m_Tab.Visible := true;

    {和事件处理有关}
    m_Tab.OnChange := g_WorkSpaceEvent.OnWorkSpaceChange;
end;

destructor TssnTabWorkSpaceMgr.Destroy;
begin
    inherited;

    m_Tab.Free();
    m_Tab := nil;
end;

function TssnTabWorkSpaceMgr.DoNewWorkSpace(var WorkSpace: TssnWorkSpace; FileName : String; nIndex : Integer): Integer;
begin
    {创建一个新的工作区
      即创建一个新的TssnTabWorkSpace实例（TTabSheet组件）}
    WorkSpace := TssnTabWorkSpace.Create(self.m_Tab, FileName, nIndex);
    
    Result := Integer(WorkSpace <> nil);
end;

procedure TssnTabWorkSpaceMgr.DoActiveWorkSpace(nIndex: Integer);
begin
    //激活一个工作区
    m_Tab.ActivePageIndex := TssnTabWorkSpace(m_WorkList[nIndex]).TabWS_GetPageIndex;
end;

{获取当前激活工作区的标识符（索引值）
  它只是把在每个TTabSheet组件的tag属性中的值取出来而已
  }
function TssnTabWorkSpaceMgr.GetActiveWorkSpaceIndex: Integer;
begin
    Result := 0;
    if m_Tab.PageCount = 0 then
        Exit;

    //还记得在创建每一个TTabSheet时，将其索引值放置在组件的tag属性里吗？    
    Result := m_Tab.ActivePage.Tag;
end;

{ TssnTabWorkSpace }

{Mgr指定管理器的组件，是一个TPageControl
  FileName给出工作区需要打开的文件名，无标题工作区则指定文件名为''
  nIndex是工作区管理器分配给新工作区的标识符}
constructor TssnTabWorkSpace.Create(Mgr : TPageControl; FileName : String; nIndex : Integer);
begin
    //首先创建TTabSheet的实例，并指定其PageControl属性
    m_TabSheet := TTabSheet.Create(nil);
    m_TabSheet.PageControl := Mgr;
    
    m_TabSheet.Align := alClient;
    m_TabSheet.Visible := true;

    //利用TTabSheet组件的tag属性，记住该工作区的逻辑标识符
    m_TabSheet.Tag := nIndex;

    //设置标题，并调用基类的构造函数
    SetCaption(FileName);
    inherited Create(m_TabSheet, FileName, nIndex);
end;

destructor TssnTabWorkSpace.Destroy;
begin
    inherited;

    m_TabSheet.Free();
    m_TabSheet := nil;
end;

procedure TssnTabWorkSpace.OnSave;
begin
    SetCaption(m_Editor.GetFileName());
end;

procedure TssnTabWorkSpace.SetCaption(FileName : String);
begin
    if FileName = '' then
        m_TabSheet.Caption := str_NoTitle
    else
        m_TabSheet.Caption := System.Copy(ExtractFileName(FileName), 1, 10);
end;

function TssnTabWorkSpace.TabWS_GetPageIndex: Integer;
begin
    Result := m_TabSheet.PageIndex;
end;

end.
