/*
 * Copyright (c) 2004 Nu Echo Inc.
 * 
 * This is free software. For terms and warranty disclaimer, see ./COPYING 
 */
package org.schemeway.plugins.schemescript.preferences;

import java.io.*;

import org.eclipse.jface.preference.*;
import org.eclipse.ui.*;
import org.schemeway.plugins.schemescript.*;

public class InterpreterPreferences extends FieldEditorPreferencePage implements IWorkbenchPreferencePage {
    public final static String PREFIX = SchemeScriptPlugin.PLUGIN_NS + ".interpreter.";
    public final static String INTERPRETER_NAME = PREFIX + "name";
    public final static String INTERPRETER_CMDLINE = PREFIX + "cmdline";
    public final static String INTERPRETER_DIR = PREFIX + "directory";

    public InterpreterPreferences() {
        super("Interpreter preferences", GRID);
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
        store.setDefault(INTERPRETER_NAME, "Scheme");
        store.setDefault(INTERPRETER_CMDLINE, "scheme");
        store.setDefault(INTERPRETER_DIR, "");
    }

    /*
     * (non-Javadoc)
     * 
     * @see org.eclipse.jface.preference.FieldEditorPreferencePage#createFieldEditors()
     */
    protected void createFieldEditors() {
        initializeDefaultPreferences(getPreferenceStore());
        addField(new StringFieldEditor(INTERPRETER_NAME, "Interpreter name:", getFieldEditorParent()));
        addField(new StringFieldEditor(INTERPRETER_CMDLINE, "Command line:", getFieldEditorParent()));
        addField(new DirectoryFieldEditor(INTERPRETER_DIR, "Working directory:", getFieldEditorParent()));
    }

    public static String getCommandLine() {
        IPreferenceStore store = SchemeScriptPlugin.getDefault().getPreferenceStore();
        return store.getString(INTERPRETER_CMDLINE);
    }
    
    public static String getInterpreterName() {
        IPreferenceStore store = SchemeScriptPlugin.getDefault().getPreferenceStore();
        return store.getString(INTERPRETER_NAME);
    }
    
    public static File getWorkingDirectory() {
        IPreferenceStore store = SchemeScriptPlugin.getDefault().getPreferenceStore();
        String dir = store.getString(INTERPRETER_DIR);
        
        if (dir == null || dir.equals(""))
            return null;
        else {
            return new File(dir);
        }
    }
}