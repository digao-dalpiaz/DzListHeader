# ListHeader

## Delphi visual control to create a listbox columns header.

When you are using listbox, and usually when assigning objects to listbox items, you want to show/draw columns on listbox, using OnDrawItem.

That's OK, but you don't have a header control to show the columns, and allow user to resize, to move, and show/hide columns.

The THeaderControl (Delphi default control) allows some of these operations, but you need to write a lot of code on each single listbox, taking care of repaint items and calculating column positions. And, of course, you don't have hide/show column function available.

So, I have been workin in this component, and I decided to share, because is very useful.

## Installing

Just add the UListHeader.pas to a package. Then build and install.

Supports Delphi XE..Delphi 10.3 Rio (using Generics and For In)

## How to use

Drop the TListHeader at Form, then drop a ListBox inside the ListHeader. You must align the listbox "alClient".

Set the ListHeader.ListBox = ListBox.

Create desired columns at ListHeader.Columns property (you should see the columns at design-time).

The main idea is to use the ListBox.OnDrawItem and call the ListHeader.DwCol to easily draw a column.

