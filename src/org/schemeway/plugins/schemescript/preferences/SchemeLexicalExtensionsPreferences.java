/*
 * Copyright (c) 2004 Nu Echo Inc.
 * 
 * This is free software. For terms and warranty disclaimer, see ./COPYING
 */
package org.schemeway.plugins.schemescript.preferences;

import org.eclipse.jface.preference.*;
import org.eclipse.ui.*;
import org.schemeway.plugins.schemescript.*;

public class SchemeLexicalExtensionsPreferences extends FieldEditorPreferencePage implements IWorkbenchPreferencePage {

    public final static String PREFIX = SchemeScriptPlugin.PLUGIN_NS + ".lexical-extensions.";
    public final static String SQUARE_BRACKETS = PREFIX + "square-brackets";
    public final static String DASH_IN_IDS     = PREFIX + "dash-in-identifiers";
    
    public final static boolean DEFAULT_FOR_BRACKETS = true;
    public final static boolean DEFAULT_FOR_DASHES = false;
    
    public SchemeLexicalExtensionsPreferences() {
        super("Scheme lexical extensions", GRID);
    }
    
    protected IPreferenceStore doGetPreferenceStore() {
        return SchemeScriptPlugin.getDefault().getPreferenceStore();
    }

    /*
     * (non-Javadoc)
     * 
     * @see org.eclipse.ui.IWorkbenchPreferencePage#init(org.eclipse.ui.IWorkbench)
     */
    public void init(IWorkbench workbench) {
    }
    

    protected void initializeDefaultPreferences(IPreferenceStore store) {
        initializeDefaults(store);
    }
    
    public static void initializeDefaults(IPreferenceStore store) {
        store.setDefault(SQUARE_BRACKETS, DEFAULT_FOR_BRACKETS);
        store.setDefault(DASH_IN_IDS, DEFAULT_FOR_DASHES);
    }
    
    protected void createFieldEditors() {
        initializeDefaultPreferences(getPreferenceStore());
        addField(new BooleanFieldEditor(SQUARE_BRACKETS, "Consider brackets as parentheses", getFieldEditorParent()));
        addField(new BooleanFieldEditor(DASH_IN_IDS, "Accept dash (#) as identifier part", getFieldEditorParent()));
    }
}