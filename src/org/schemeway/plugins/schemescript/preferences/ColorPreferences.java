/*
 * Copyright (c) 2004 Nu Echo Inc.
 * 
 * This is free software. For terms and warranty disclaimer, see ./COPYING
 */
package org.schemeway.plugins.schemescript.preferences;

import org.eclipse.jface.preference.*;
import org.eclipse.ui.*;

import org.schemeway.plugins.schemescript.*;
import org.schemeway.plugins.schemescript.editor.*;

public class ColorPreferences extends FieldEditorPreferencePage implements IWorkbenchPreferencePage {
    public final static String PREFIX = SchemeScriptPlugin.PLUGIN_NS + ".colors.";
    public final static String BACKGROUND_COLOR = PREFIX + "background";
    public final static String MATCHER_COLOR = PREFIX + "matcher";
    public final static String MATCHER_BOX = PREFIX + "matcherbox";

    public final static String DEFAULT_COLOR = PREFIX + "default";
    public final static String PAREN_COLOR = PREFIX + "parenthesis";
    public final static String COMMENT_COLOR = PREFIX + "comment";
    public final static String DEFINE_COLOR = PREFIX + "define";
    public final static String KEYWORD_COLOR = PREFIX + "keyword";
    public final static String KEY_COLOR = PREFIX + "key";
    public final static String SPECIAL_COLOR = PREFIX + "special";
    public final static String ERROR_COLOR = PREFIX + "error";
    public final static String STRING_COLOR = PREFIX + "string";
    public final static String CONSTANT_COLOR = PREFIX + "constant";
    public final static String MUTATOR_COLOR = PREFIX + "mutator";
    public final static String TYPE_COLOR = PREFIX + "type";

    public ColorPreferences() {
        super("Scheme editor colors", GRID);
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
        PreferenceConverter.setDefault(store, MATCHER_COLOR, ISchemeColorConstants.MATCHING_PARENS);
        store.setDefault(MATCHER_BOX, false);

        PreferenceConverter.setDefault(store, DEFAULT_COLOR, ISchemeColorConstants.SCHEME_DEFAULT);
        PreferenceConverter.setDefault(store, BACKGROUND_COLOR, ISchemeColorConstants.SCHEME_BACKGROUND);
        PreferenceConverter.setDefault(store, PAREN_COLOR, ISchemeColorConstants.SCHEME_PAREN);
        PreferenceConverter.setDefault(store, COMMENT_COLOR, ISchemeColorConstants.SCHEME_COMMENT);
        PreferenceConverter.setDefault(store, DEFINE_COLOR, ISchemeColorConstants.SCHEME_DEFINE);
        PreferenceConverter.setDefault(store, KEYWORD_COLOR, ISchemeColorConstants.SCHEME_KEYWORD);
        PreferenceConverter.setDefault(store, KEY_COLOR, ISchemeColorConstants.SCHEME_KEY);
        PreferenceConverter.setDefault(store, SPECIAL_COLOR, ISchemeColorConstants.SCHEME_SPECIAL);
        PreferenceConverter.setDefault(store, STRING_COLOR, ISchemeColorConstants.SCHEME_STRING);
        PreferenceConverter.setDefault(store, CONSTANT_COLOR, ISchemeColorConstants.SCHEME_CONSTANT);
        PreferenceConverter.setDefault(store, MUTATOR_COLOR, ISchemeColorConstants.SCHEME_MUTATOR);
        PreferenceConverter.setDefault(store, TYPE_COLOR, ISchemeColorConstants.SCHEME_TYPE);
        PreferenceConverter.setDefault(store, ERROR_COLOR, ISchemeColorConstants.SCHEME_ERROR);
    }

    /*
     * (non-Javadoc)
     * 
     * @see org.eclipse.jface.preference.FieldEditorPreferencePage#createFieldEditors()
     */
    protected void createFieldEditors() {
        initializeDefaultPreferences(getPreferenceStore());
        addField(new ColorFieldEditor(DEFAULT_COLOR, "Default color:", getFieldEditorParent()));
        addField(new ColorFieldEditor(BACKGROUND_COLOR, "Background color:", getFieldEditorParent()));
        addField(new ColorFieldEditor(PAREN_COLOR, "Parentheses color:", getFieldEditorParent()));
        addField(new ColorFieldEditor(COMMENT_COLOR, "Comments color:", getFieldEditorParent()));
        addField(new ColorFieldEditor(DEFINE_COLOR, "Defining form color:", getFieldEditorParent()));
        addField(new ColorFieldEditor(SPECIAL_COLOR, "Special forms color:", getFieldEditorParent()));
        addField(new ColorFieldEditor(KEY_COLOR, "Keywords color:", getFieldEditorParent()));
        addField(new ColorFieldEditor(KEYWORD_COLOR, "Special named constants color:", getFieldEditorParent()));
        addField(new ColorFieldEditor(STRING_COLOR, "Strings color:", getFieldEditorParent()));
        addField(new ColorFieldEditor(CONSTANT_COLOR, "Constants color:", getFieldEditorParent()));
        addField(new ColorFieldEditor(MUTATOR_COLOR, "Mutators color:", getFieldEditorParent()));
        addField(new ColorFieldEditor(TYPE_COLOR, "Types color:", getFieldEditorParent()));
        addField(new ColorFieldEditor(ERROR_COLOR, "Error color:", getFieldEditorParent()));
        addField(new ColorFieldEditor(MATCHER_COLOR, "Matched parenthesis color:", getFieldEditorParent()));
        addField(new BooleanFieldEditor(MATCHER_BOX, "Draw box around matching parenthesis", getFieldEditorParent()));
    }
}