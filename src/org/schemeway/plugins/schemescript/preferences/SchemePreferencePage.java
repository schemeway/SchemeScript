/*
 * Copyright (c) 2004 Nu Echo Inc.
 * 
 * This is free software. For terms and warranty disclaimer, see ./COPYING
 */
package org.schemeway.plugins.schemescript.preferences;

import org.eclipse.jface.preference.*;
import org.eclipse.ui.*;
import org.schemeway.plugins.schemescript.*;

public abstract class SchemePreferencePage extends PreferencePage implements IWorkbenchPreferencePage {

    public void init(IWorkbench workbench) {
    }

    protected IPreferenceStore doGetPreferenceStore() {
        return SchemeScriptPlugin.getDefault().getPreferenceStore();
    }

    public boolean performOk() {
        storeValues();
        return true;
    }
    
    public void performDefaults() {
        super.performDefaults();

        doPerformDefaults();
        storeValues();
    }
    
    protected abstract void storeValues();
    protected abstract void doPerformDefaults();
    protected abstract void initializeValues();
}
