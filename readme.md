# ListHeader

## Delphi visual control to create a listbox columns header

- [Component Description](#component-description)
- [Installing](#installing)
- [How to use](#how-to-use)
- [OnDrawItem usage](#ondrawitem-usage)
- [Customize Dialog](#customize-dialog)
- [Properties](#properties)
- [Column properties](#column-properties)
- [Procedures/Functions](#proceduresfunctions)
- [Column Procedures/Functions](#column-proceduresfunctions)
- [Events](#events)

## Component Description

When you are using listbox, and usually when assigning objects to listbox items, you want to show/draw columns in the listbox, using OnDrawItem event.

That's OK, but you don't have a header control to show the columns, and neither allow user to resize, to move, and show/hide columns.

The THeaderControl (Delphi default control) allows some of these operations, but you need to write lots of code for every single listbox, taking care of repainting items and calculating columns positions. And, of course, you don't have hide/show column function available.

So, I have been working on this component, and I decided to share it, because it's very useful.

In addition, the ListHeader displays a scroll bar when the columns oversizes the visible area. :smile:

## Installing

Just add the UListHeader.pas to a package. Then build and install.

Note: To ensure that the component is displayed with its icon, add the following line to the Package Source:
```
{$R UListHeader.dcr}
```

Supports Delphi XE2..Delphi 10.3 Rio

## How to use

Drop the ListHeader in a Form, then drop a ListBox inside the ListHeader.

**Set the ListHeader.ListBox = ListBox.**

Create desired columns at ListHeader.Columns property (you should see the columns at design-time).

Then write OnDrawItem of ListHeader (not OnDrawItem of ListBox - leave this unassigned). Call the method ListHeader.DwCol to easily draw each single column.

Please see Example folder to know the basic functions.

![Example Image](print.png?raw=true "Example Application")
![Example CustomDlg](custom_dlg.png?raw=true "Example Custom Dlg")

## OnDrawItem usage

You should write OnDrawItem of ListHeader (not ListBox), following this idea:

```delphi
procedure TForm1.ListHeaderDrawItem(Control: TWinControl; Index: Integer; Rect: TRect;
  State: TOwnerDrawState);
var P: TPerson;
begin
  P := TPerson(ListBox.Items.Objects[Index]);

  ListHeader.DwCol(0, Rect, P.ID);
  ListHeader.DwCol(1, Rect, P.Name);
  ListHeader.DwCol(2, Rect, P.Gender);
end;
```
Simply like that!

## Working with Tabbed Text

This components offers a second option of usage, so you don't need to use objects, and you don't need to write code to draw items.
In this option, you should add items with text cells separated by Tab character.

Example:
```
1<TAB>
```

## Customize Dialog

To open Customize Dialog, just right-click at any part of header or columns.
In Customize Dialog you can reorder columns, show/hide columns and set the default order.

## Properties

`AllowMoving` = Enable/Disable columns repositioning

`AllowResize` = Enable/Disable columns moving (if false, Column.Sizeable doesn't matters)

`AutoDrawTabbedText` = When using AutoDrawTabbedText, you don't need to use objects or OnDrawItem. You may add itens to the ListBox with Tab dellimited to split columns. In this case, you don't need to code to draw items. There is two methos available to use with tabbed text: AddItem (add item array, so the component automatically convert in tabbed text); GetArrayText (returns an array of requested index).
*You can still leave this property disabled and work with tabbed text, writing event OnDrawItem and using GetArrayText to read text of columns separated.*

`ColorNormalCol` = Column background color

`ColorHoverCol` = Column background color when mouse over the column

`ColorLineNormal` = Line background color (on listbox) when not odd line (or UseOdd disabled), and not selected line

`ColorLineOdd` = Line background color when odd line and UseOdd enabled

`ColorLineSel` = Line background color when line is selected

`ColorShape` = Color of dash that's indicate moving or resising orientation

`Columns` = Columns Collection

`HeaderHeight` = Fixed Header Height (you can type multiple-lines in Column Caption if you want)

`LineCenter` = indicates DwCol function draws text centralized vertically according to the line height

`LineTop` = indicates the Y position of text to the DwCol when LineCenter is False

`ListBox` = *ListBox object (required!)*

`TextMargin` = Space in Pixels at left and right of column (used so that the text in one column does not stick to the text in another column)

`TitleFont` = Title Font for Columns captions

`UseOdd` = use specific color background for odd lines (see ColorLineOdd property)

## Column properties

`Alignment` = Indicates alignment of text used on DwCol function

`Caption` = The caption text of column title

`CaptionEx` = This caption is optional, used if you want to specify a full caption to display in Customize Dialog (e.g.: Caption: "ID", CaptionEx: "ID of Person")

`Customizable` = Allow the column to be customizable on Customize Dialog

`CustomTextFont` = Determines stored for TextFont property (is automatically set when TextFont changes)

`Hint` = Column Hint

`MaxWidth` = Column MaxWidth when resizing

`MinWidth` = Column MinWidth when resizing

`Name` = Column Name to find the column (ColByName function) and for Save/Load customization (SaveCustom/LoadCustom functions).
*The customization requires column name because you may change your project, and in this case the columns will be keeped in correct order based on columns names.*

`Sizeable` = Allow column resize

`TextFont` = Font used by Canvas to draw item text for this column (if not changed, canvas uses ListBox font to draw items)

`Visible` = Show/Hide column

`Width` = Column width

`Data` = Pointer to free use (non published property)

## Procedures/Functions

```
procedure LoadCustom(const A: String)
```
Loads column customization from string, including position, size, and visibility.
You can load from registry/ini file.

```
function SaveCustom: String
```
Save columns customization to string, including position, size, and visibility.
You can save to registry/ini file.

```
function ColByID(ID: Integer): TListHeaderCol
```
Returns a TListHeaderCol by column ID. The ID remains fixed when moving columns (the position of column is defined by Index property).

```
function ColByName(const aName: String): TListHeaderCol
```
Returns a TListHeaderCol by column Name.

```
procedure DwCol(ID: Integer; Rect: TRect; const Value: Variant; Margin: Integer = 0)
```
Used at OnDrawItem, to draw a column item text.
The ID represents the column number considering the order in collection. So, even when the columns are moved at run-time, the ID remains always the same. This is the main identification of the column.
The value represents the text to be printed. Note that the value is variant type, so you don't need to convert to string, unless you want to format the value, of course.
You can specify a margin at left side of column, to draw an icon or other custom draw (see example source).

```
function AddItem(const Ar: TArray<String>): Integer
```
This function helps you to add item to ListBox, automatically separating array strings with tab character. You should use this function only when you are storing data into ListBox using tabbed delimiter method. Usually you will want to use this along with the AutoDrawTabbedText property, so you don't need to write OnDrawItem.

```
function GetItemArray(Index: Integer): TArray<String>
```
Returns an array of strings relative to the Index in ListBox, considering item text has data separated by tab character.
You can quickly read a cell using `GetItemArray(Index)[Column]`.

## Column Procedures/Functions

```
function GetLeft: Integer
```
Returns left position of column according by rect bounds.

```
function GetRight: Integer
```
Returns right position of column according by rect bounds.

## Events

```
OnColumnClick(Sender: TObject; Col: TListHeaderCol)
```
Occurs when left-clicked on a column.

```
OnColumnRClick(Sender: TObject; Col: TListHeaderCol)
```
Occurs when right-clicked on a column.

```
OnColumnResize(Sender: TObject; Col: TListHeaderCol)
```
Occurs after a column was resised.

```
OnDrawItem(Control: TWinControl; Index: Integer; Rect: TRect; State: TOwnerDrawState)
```
You should used this event to write all columns, using DwCol method (please see example source).
> **This event won't fire if you are using AutoDrawTabbedText=True**

```
MouseEnterCol(Sender: TObject; Col: TListHeaderCol)
```
Ocurrs when mouse enters a column area.

```
MouseLeaveCol(Sender: TObject; Col: TListHeaderCol)
```
Ocurrs when mouse leaves a column area.
