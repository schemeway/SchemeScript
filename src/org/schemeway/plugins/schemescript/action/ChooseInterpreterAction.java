/*
 * Copyright (c) 2004 Nu Echo Inc.
 * 
 * This is free software. For terms and warranty disclaimer, see ./COPYING
 */
package org.schemeway.plugins.schemescript.action;

import org.eclipse.jface.action.*;
import org.eclipse.jface.viewers.*;
import org.eclipse.swt.*;
import org.eclipse.swt.events.*;
import org.eclipse.swt.widgets.*;
import org.eclipse.ui.*;
import org.schemeway.plugins.schemescript.*;
import org.schemeway.plugins.schemescript.interpreter.*;

public class ChooseInterpreterAction implements IWorkbenchWindowPulldownDelegate2 {

    private Menu     mMainMenu = null;
    private MenuItem mSelectedItem = null;
    
    public ChooseInterpreterAction() {
        super();
    }

    public void dispose() {
        if (mMainMenu != null)
            mMainMenu.dispose();
    }

    public void init(IWorkbenchWindow window) {
    }

    public Menu getMenu(Control parent) {
        return null;
    }
    public Menu getMenu(Menu parent) {
        InterpreterType[] types = InterpreterSupport.getTypes();
        Menu menu = new Menu(parent);
        for (int i = 0; i < types.length; i++) {
            final InterpreterType type = types[i];
            final MenuItem item = new MenuItem(menu, SWT.CHECK);
            item.setText(type.getName());
            if (type == SchemeScriptPlugin.getDefault().getCurrentInterpreterType()) {
                mSelectedItem = item;
                item.setSelection(true);
            }
            item.addSelectionListener(new SelectionAdapter() {
                public void widgetSelected(SelectionEvent event) {
                    item.setSelection(true);
                    if (mSelectedItem != null)
                        mSelectedItem.setSelection(false);
                    mSelectedItem = item;
                    SchemeScriptPlugin.getDefault().setInterpreter(type);
                }
            });
        }
        mMainMenu = menu;
        return menu;
    }

    public void run(IAction action) {
    }

    public void selectionChanged(IAction action, ISelection selection) {
    }
}
