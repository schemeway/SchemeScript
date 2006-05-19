/*
 * Copyright (c) 2004 Nu Echo Inc.
 * 
 * This is free software. For terms and warranty disclaimer, see ./COPYING
 */
package org.schemeway.plugins.schemescript.dialogs;

import java.util.TreeSet;

import org.eclipse.jface.dialogs.*;
import org.eclipse.jface.dialogs.Dialog;
import org.eclipse.swt.*;
import org.eclipse.swt.events.*;
import org.eclipse.swt.layout.*;
import org.eclipse.swt.widgets.*;
import org.schemeway.plugins.schemescript.dictionary.*;

public final class FindSymbolDialog extends Dialog {
    
    private ISymbolDictionary mDictionary;
    private SymbolEntry[] mSelectedEntries = null;
    
    private Text mPrefix;
    private List mSymbols;

    public FindSymbolDialog(Shell parentShell, ISymbolDictionary dictionary) {
        super(parentShell);
        mDictionary = dictionary;
        setShellStyle(getShellStyle() | SWT.RESIZE);
    }
    
    
    protected void configureShell(Shell newShell)
    {
        super.configureShell(newShell);
        newShell.setText("Find Symbol");
    }
    
    protected Control createDialogArea(Composite parent)
    {
        Composite composite  = (Composite) super.createDialogArea(parent);
        GridData data;
        new Label(composite, SWT.NONE).setText("Enter the symbol prefix:");
        mPrefix = new Text(composite, SWT.SINGLE|SWT.BORDER);
        data = new GridData(GridData.FILL_HORIZONTAL);
        mPrefix.setLayoutData(data);
        mPrefix.addModifyListener(new ModifyListener() {
            public void modifyText(ModifyEvent event) {
                updateSymbols();
            }
        });
        
        new Label(composite, SWT.NONE).setText("Matching symbols:");
        mSymbols = new List(composite, SWT.SINGLE|SWT.BORDER|SWT.V_SCROLL|SWT.H_SCROLL);
        data = new GridData(GridData.FILL_BOTH);
        data.heightHint = 100;
        data.widthHint = 200;
        mSymbols.setLayoutData(data);
        mSymbols.addSelectionListener(new SelectionAdapter() {
            public void widgetSelected(SelectionEvent event) {
                getButton(IDialogConstants.OK_ID).setEnabled(mSymbols.getSelectionCount() != 0);
            }
        });
        
        return composite;
    }
    
    private void updateSymbols() {
        String text = mPrefix.getText();
        mSymbols.removeAll();
        if (!text.equals("")) {
            SymbolEntry[] entries = mDictionary.completeSymbol(text);
            TreeSet set = new TreeSet();
            
            for(int i=0; i<entries.length; i++) {
                if (entries[i].getFile() != null) 
                    set.add(entries[i].getName());
            }
            
            String[] names = (String[])set.toArray(new String[set.size()]);
            
            for(int i=0; i<names.length; i++) {
                mSymbols.add(names[i]);
            }
        }
    }
    
    protected void okPressed() {
        if (mSymbols.getSelectionCount() == 1) {
            int selection = mSymbols.getSelectionIndices()[0];
            String symbol = mSymbols.getItem(selection);
            mSelectedEntries = mDictionary.findSymbol(symbol);
        }
        else if (mSymbols.getItemCount() > 0)
        {
        	String symbol = mSymbols.getItem(0);
        	mSelectedEntries = mDictionary.findSymbol(symbol);
        }
        else {
            mSelectedEntries = null;
        }
        super.okPressed();
    }
    
    public SymbolEntry[] getSelectedEntries() {
        return mSelectedEntries;
    }
}
