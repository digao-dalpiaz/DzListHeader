# ListHeader

## Delphi visual control to create a listbox columns header.

When you are using listbox, and usually when assigning objects to listbox items, you want to show/draw columns on listbox, using OnDrawItem.

That's OK, but you don't have a header control to show the columns, and allow user to resize, to move, and show/hide columns.

The THeaderControl (Delphi default control) allows some of these operations, but you need to write a lot of code on each single listbox, taking care of repaint items and calculating column positions. And, of course, you don't have hide/show column function available.

So, I have been workin in this component, and I decided to share, because is very useful.

Besides that the ListHeader shows a scroll bar when the columns oversizes the visible area. :)

## Installing

Just add the UListHeader.pas to a package. Then build and install.

Supports Delphi XE..Delphi 10.3 Rio (using Generics and For In)

## How to use

Drop the TListHeader at Form, then drop a ListBox inside the ListHeader. You must align the listbox "alClient".

Set the ListHeader.ListBox = ListBox.

Create desired columns at ListHeader.Columns property (you should see the columns at design-time).

The main idea is to use the ListBox.OnDrawItem and call the ListHeader.DwCol to easily draw a column.

Please see Example folder to know the basic functions.

![Example Image](print.png?raw=true "Example Application")

## Properties

`AllowMoving` = Enable/Disable columns repositioning

`AllowResize` = Enable/Disable columns moving (if false, Column.Sizeable doesn't matters)

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

`TitleFont` = Title Font for Columns captions

`UseOdd` = use specific color background for odd lines (see ColorLineOdd property)

## Columns properties

`Alignment` = Indicates alignment of text used on DwCol function

`Caption` = The caption text of column title

`CaptionEx` = This caption is optional, used if you want to specify a full caption to display in Customize Dialog (e.g.: Caption: "ID", CaptionEx: "ID of Person")

`Customizable` = Allow the column to be customizable on Customize Dialog

`Hint` = Column Hint

`MaxWidth` = Column MaxWidth when resizing

`MinWidth` = Column MinWidth when resizing

`Name` = Column Name to find the column (ColByName function) and for Save/Load customization (SaveCustom/LoadCustom functions)

`Sizeable` = Allow column resize

`Visible` = Show/Hide column

`Width` = Column width

## Procedures/Functions

```
procedure LoadCustom(const A: String)
```
Loads column customization from string, including position, size, and visibility
You can load from registry/ini file

```
function SaveCustom: String
```
Save columns customization to string, including position, size, and visibility
You can save to registry/ini file

```
function ColByID(ID: Integer): TListHeaderCol
```
Returns a TListHeaderCol by column ID. The ID remains fixed when moving columns (the position of column is defined by Index property).

```
function ColByName(const aName: String): TListHeaderCol
```
Returns a TListHeaderCol by column Name.

```
procedure InitDrawItem(Index: Integer; Rect: TRect; State: TOwnerDrawState)
```
Used at OnDrawItem of ListBox, to initilize line drawing

```
procedure DwCol(ID: Integer; Rect: TRect; const Value: Variant; Margin: Integer = 0)
```
Used at OnDrawItem, to draw a column.
You can specify a margin at left side of column, to draw an icon or other custom draw.

## OnDrawItem usage

You should write OnDrawItem of TListBox following this idea:

```
procedure TForm1.ListBoxDrawItem(Control: TWinControl; Index: Integer; Rect: TRect;
  State: TOwnerDrawState);
var P: TPerson;
begin
  ListHeader.InitDrawItem(Index, Rect, State);

  P := TPerson(ListBox.Items.Objects[Index]);

  ListHeader.DwCol(0, Rect, P.ID);
  ListHeader.DwCol(1, Rect, P.Name);
  ListHeader.DwCol(2, Rect, P.Gender);
end;
```
