/*
 * Copyright (c) 2004 Nu Echo Inc.
 * 
 * This is free software. For terms and warranty disclaimer, see ./COPYING 
 */
package org.schemeway.plugins.schemescript.preferences;

import org.eclipse.jface.preference.*;
import org.eclipse.ui.*;
import org.schemeway.plugins.schemescript.*;

public class FastEvalPreferences extends FieldEditorPreferencePage implements IWorkbenchPreferencePage {
    public final static String PREFIX = SchemeScriptPlugin.PLUGIN_NS + ".interpreter.";
    public final static String FASTEVAL_0 = PREFIX + "0";
    public final static String FASTEVAL_1 = PREFIX + "1";
    public final static String FASTEVAL_2 = PREFIX + "2";
    public final static String FASTEVAL_3 = PREFIX + "3";
    public final static String FASTEVAL_4 = PREFIX + "4";
    public final static String FASTEVAL_5 = PREFIX + "5";
    public final static String FASTEVAL_6 = PREFIX + "6";
    public final static String FASTEVAL_7 = PREFIX + "7";
    public final static String FASTEVAL_8 = PREFIX + "8";
    public final static String FASTEVAL_9 = PREFIX + "9";

    public FastEvalPreferences() {
        super("Fast Keys", GRID);
    }

    public void init(IWorkbench workbench) {
    }

    protected IPreferenceStore doGetPreferenceStore() {
        return SchemeScriptPlugin.getDefault().getPreferenceStore();
    }

    protected void initializeDefaultPreferences(IPreferenceStore store) {
        initializeDefaults(store);
    }

    public static void initializeDefaults(IPreferenceStore store) {
        for (int i=0; i<10; i++) {
            store.setDefault(PREFIX + i, "");
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see org.eclipse.jface.preference.FieldEditorPreferencePage#createFieldEditors()
     */
    protected void createFieldEditors() {
        initializeDefaultPreferences(getPreferenceStore());
        for (int i=0; i<10; i++) {
            addField(new StringFieldEditor(PREFIX + i, "Fast Eval Key " + i + ":", getFieldEditorParent()));
        }
    }

    
    public static String getFastEval(int index) {
        IPreferenceStore store = SchemeScriptPlugin.getDefault().getPreferenceStore();
        if (0 <= index && index <= 9) {
            return store.getString(PREFIX + index);
        }
        return "";
    }
}