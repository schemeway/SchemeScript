/*
 * Copyright (c) 2004 Nu Echo Inc.
 * 
 * This is free software. For terms and warranty disclaimer, see ./COPYING
 */
package org.schemeway.plugins.schemescript.dialogs;

import org.eclipse.jface.window.*;
import org.eclipse.swt.*;
import org.eclipse.swt.events.*;
import org.eclipse.swt.layout.*;
import org.eclipse.swt.widgets.*;
import org.schemeway.plugins.schemescript.dictionary.*;

public class SymbolSelectionDialog extends Window {
    private SymbolEntry[] mEntries;
    private SymbolEntry mSelectedEntry = null;

    public SymbolSelectionDialog(Shell parentShell, SymbolEntry[] entries) {
        super(parentShell);
        setShellStyle(SWT.APPLICATION_MODAL);
        setBlockOnOpen(true);
        mEntries = entries;
        if (entries.length > 0) {
            mSelectedEntry = mEntries[0];
        }
    }

    public SymbolEntry getSelectedEntry() {
        return mSelectedEntry;
    }

    protected Control createContents(Composite parent) {
        Composite composite = new Composite(parent, SWT.NULL);
        GridLayout layout = new GridLayout(1, true);
        layout.verticalSpacing = 0;
        layout.marginHeight = 0;
        layout.marginWidth = 0;
        composite.setLayout(layout);
        
        Label label = new Label(composite, SWT.BORDER | SWT.SHADOW_IN);
        label.setText(" Symbols");
        GridData data = new GridData(GridData.FILL_HORIZONTAL);
        label.setLayoutData(data);
        
        
        final List list = new List(composite, SWT.SINGLE);
        data = new GridData(GridData.FILL_BOTH);
        list.setLayoutData(data);

        for (int i = 0; i < mEntries.length; i++) {
            list.add(mEntries[i].getDescription() + " [" + mEntries[i].getContext() + "]");
        }
        list.select(0);

        list.addSelectionListener(new SelectionAdapter() {
            public void widgetSelected(SelectionEvent event) {
                int index = list.getSelectionIndex();
                mSelectedEntry = mEntries[index];
            }
        });
        list.addKeyListener(new KeyAdapter() {
            public void keyPressed(KeyEvent event) {
                if (event.character == '\r') {
                    ok();
                }
            }
        });
        list.addMouseListener(new MouseAdapter() {
            public void mouseDoubleClick(MouseEvent event) {
                ok();
            }
        });
        return composite;
    }

    protected void ok() {
        setReturnCode(OK);
        close();
    }

    protected void cancel() {
        mSelectedEntry = null;
        close();
    }
}