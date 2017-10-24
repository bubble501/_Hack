////////////////////////////////////////////////////////////////////////////////
//
//
//  FileName    :   EditorEvent.pas
//  Creator     :   Shen Min
//  Date        :   2002-4-16
//  Comment     :
//
//
////////////////////////////////////////////////////////////////////////////////

unit EditorEvent;

{编辑器组件需要事件委托，其需要委托的事件只有一个，即编辑器状态的更新
  当其任何一个状态发生变化时，都通过一个事件通知界面层做出响应
  当然，也可以将每个状态的改变拆分成多个事件
更详细的解释，还是看《Delphi高手突破》书中的解释
  }

interface

uses Classes;

type
    {在此处，编辑器组件的状态只包括了是否允许复制、剪切等，而且逻辑上同属于编辑器的选择状态，
      界面模块的响应程序可以对这类事件做同一件事——取编辑器的选中状态，刷新菜单
      因此，可以将它们归到同一个事件中：OnEditorSelectionChange
      }
    {由于它只管理一个事件，因此只有两个public方法，一个由界面层调用设置其回调函数指针
      另一个由编辑器组件调用以通知其状态更新
      当然，其内部还有一个private的函数指针类型的数据成员，以保存界面层提供给它的回调函数指针
      }
    TssnEditorEvent = class
    private
        m_OnEditorChange : TNotifyEvent;    //回调函数指针

    public
        procedure OnEditorSelectionChange(Sender : TObject);

        procedure SetOnEditorSelectionChange(Value : TNotifyEvent); //界面层模块通过调用SetOnEditorSelectionChange方法来设置回调函数指针
    end;

implementation

{ TssnEditorEvent }

{当编辑器的可编辑状态发生变化时，调用OnEditorChange来通知TssnEditorEvent类的对象
  然后由TssnEditorEvent的实例对象调用回调函数通知界面模块做出响应
  }
procedure TssnEditorEvent.OnEditorSelectionChange(Sender : TObject);
begin
    if Assigned(m_OnEditorChange) then
        m_OnEditorChange(Sender);     //调用回调函数
end;

{界面层模块通过调用SetOnEditorSelectionChange方法来设置回调函数指针}
procedure TssnEditorEvent.SetOnEditorSelectionChange(Value : TNotifyEvent);
begin
    m_OnEditorChange := Value;
end;

end.
 