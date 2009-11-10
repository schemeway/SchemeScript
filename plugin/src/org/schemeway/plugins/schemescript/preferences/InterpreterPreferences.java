/*
 * Copyright (c) 2004 Nu Echo Inc.
 * 
 * This is free software. For terms and warranty disclaimer, see ./COPYING
 */
package org.schemeway.plugins.schemescript.preferences;

import org.eclipse.jface.preference.*;
import org.eclipse.swt.*;
import org.eclipse.swt.events.*;
import org.eclipse.swt.layout.*;
import org.eclipse.swt.widgets.*;
import org.schemeway.plugins.schemescript.*;

public class InterpreterPreferences extends SchemePreferencePage {

    private final static String PREFIX = SchemeScriptPlugin.PLUGIN_NS + ".interpreter.";
    public final static String SAVE_BEFORE_LOAD = PREFIX + "saveBeforeLoad";
    public final static String SURROUND_WITH_BEGIN = PREFIX + "surroundWithBegin";

    private final static boolean DEFAULT_SAVE_BEFORE_LOAD = true;
    private final static boolean DEFAULT_SURROUND_WITH_BEGIN = true;
    
    private boolean mSaveBeforeLoad; 
    private boolean mSurroundWithBegin; 
    
    protected Control createContents(Composite parent) {
        initializeValues();
        Composite composite = new Composite(parent, SWT.NULL);
        composite.setLayout(new GridLayout(1, false));
        composite.setLayoutData(new GridData(GridData.FILL_BOTH));

        createControls(composite);

        return getControl();
    }

    private void createControls(Composite parent) {
        Group container = new Group(parent, SWT.SHADOW_ETCHED_OUT);
        container.setText("General interpreter properties");
        GridData data = new GridData(GridData.FILL_HORIZONTAL | GridData.VERTICAL_ALIGN_BEGINNING);
        container.setLayoutData(data);
        container.setLayout(new GridLayout(1, false));
    
        final Button saveButton = new Button(container, SWT.CHECK);
        saveButton.setText("Save file before loading in interpreter");
        saveButton.addSelectionListener(new SelectionAdapter() {
            public void widgetSelected(SelectionEvent event) {
                mSaveBeforeLoad = saveButton.getSelection();
            }
        });
        saveButton.setSelection(mSaveBeforeLoad);

        final Button surroundButton = new Button(container, SWT.CHECK);
        surroundButton.setText("Surround expressions with (begin ...  )");
        surroundButton.addSelectionListener(new SelectionAdapter() {
            public void widgetSelected(SelectionEvent event) {
                mSurroundWithBegin = surroundButton.getSelection();
            }
        });
        surroundButton.setSelection(mSurroundWithBegin);

    }
    
    protected void doPerformDefaults() {
        mSurroundWithBegin = DEFAULT_SURROUND_WITH_BEGIN;
        mSaveBeforeLoad = DEFAULT_SAVE_BEFORE_LOAD;
    }
    
    public static void initializeDefaults(IPreferenceStore store) {
        store.setDefault(SURROUND_WITH_BEGIN, DEFAULT_SURROUND_WITH_BEGIN);
        store.setDefault(SAVE_BEFORE_LOAD, DEFAULT_SAVE_BEFORE_LOAD);
    }
    
    protected void initializeValues() {
        IPreferenceStore store = getPreferenceStore();
        mSaveBeforeLoad = store.getBoolean(SAVE_BEFORE_LOAD);
        mSurroundWithBegin = store.getBoolean(SURROUND_WITH_BEGIN);
    }
    
    protected void storeValues() {
        IPreferenceStore store = getPreferenceStore();
        store.setValue(SURROUND_WITH_BEGIN, mSurroundWithBegin);
        store.setValue(SAVE_BEFORE_LOAD, mSaveBeforeLoad);
    }
    
    public static boolean surroundCodeWithBegin() {
        return SchemeScriptPlugin.getDefault().getPreferenceStore().getBoolean(SURROUND_WITH_BEGIN);
    }
    
    public static boolean isSaveRequiredBeforeLoad() {
        return SchemeScriptPlugin.getDefault().getPreferenceStore().getBoolean(SAVE_BEFORE_LOAD);
    }

}