////////////////////////////////////////////////////////////////////////////////
//
//
//  FileName    :   WorkSpaceEvent.pas
//  Creator     :   Shen Min
//  Date        :   2002-4-16
//  Comment     :
//
//
////////////////////////////////////////////////////////////////////////////////

unit WorkSpaceEvent;

{同样，工作区状态也会变化（比如新打开一个工作区、关闭一个工作区等），工作区对象也需要和界面层模块进行交互}
{由于前面已经有了TssnEditorEvent的设计经验，而设计TssnWorkSpaceEvent也是类似的工作，因此很容易可以知道该去怎么做}
{首先，确定工作区状态发生变化的种类
  一类是工作区数量发生了变化，如打开、关闭一个或多个工作区
    例如打开的工作区数目变为0时，必须使菜单中如“关闭工作区”等菜单选项变为不用状态
  另一类是当前工作区被改变了
    如用户激活了另一个工作区，那么状态栏上的文件名必须做出响应的改变
  这两类时间对于界面层来说，是无法以一种方式来处理的
    所以为TssnWorkSpaceEvent定义两个事件
    界面层通过调用SetOnWorkSpaceXXX方法设置响应事件的回调函数指针
    而工作区管理器对象与工作区对象调用OnWorkSpaceXXX来激活事件
  }


interface

uses Classes;

type
    TssnWorkSpaceEvent = class
    private
        m_OnWorkSpaceOpenClose : TNotifyEvent;
        m_OnWorkSpaceChange : TNotifyEvent;

    public
        procedure OnWorkSpaceOpenClose(Sender : TObject);
        procedure OnWorkSpaceChange(Sender : TObject);

        procedure SetOnWorkSpaceOpenClose(Value : TNotifyEvent);
        procedure SetOnWorkSpaceChange(Value : TNotifyEvent);
    end;

implementation

{ TssnWorkSpaceEvent }

procedure TssnWorkSpaceEvent.OnWorkSpaceChange(Sender: TObject);
begin
    if Assigned(m_OnWorkSpaceChange) then
        m_OnWorkSpaceChange(Sender);
end;

procedure TssnWorkSpaceEvent.OnWorkSpaceOpenClose(Sender: TObject);
begin
    if Assigned(m_OnWorkSpaceOpenClose) then
        m_OnWorkSpaceOpenClose(Sender);
end;

procedure TssnWorkSpaceEvent.SetOnWorkSpaceChange(Value: TNotifyEvent);
begin
    m_OnWorkSpaceChange := Value;
end;

procedure TssnWorkSpaceEvent.SetOnWorkSpaceOpenClose(Value: TNotifyEvent);
begin
    m_OnWorkSpaceOpenClose := Value;
end;

end.
 