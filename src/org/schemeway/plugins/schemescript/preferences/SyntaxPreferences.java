/*
 * Copyright (c) 2004 Nu Echo Inc.
 * 
 * This is free software. For terms and warranty disclaimer, see ./COPYING
 */
package org.schemeway.plugins.schemescript.preferences;

import org.eclipse.jface.preference.*;
import org.eclipse.jface.viewers.*;
import org.eclipse.swt.*;
import org.eclipse.swt.events.*;
import org.eclipse.swt.layout.*;
import org.eclipse.swt.widgets.*;
import org.eclipse.ui.*;

import org.schemeway.plugins.schemescript.*;
import org.schemeway.plugins.schemescript.parser.*;

public class SyntaxPreferences extends SchemePreferencePage {
    public final static String PREFIX = SchemeScriptPlugin.PLUGIN_NS + ".syntax.";
    public final static String SYNTAX_KEYWORD = PREFIX + "keywords";
    public final static String SYNTAX_DEFINE = PREFIX + "define";
    public final static String SYNTAX_SPECIAL = PREFIX + "special";
    public final static String SYNTAX_MUTATOR = PREFIX + "mutator";
    public final static String SYNTAX_CONSTANT = PREFIX + "constant";

    private List mDefineList;
    private List mKeywordList;
    private List mSpecialList;
    private List mMutatorList;
    private List mConstantList;

    private final static String[] DEFAULT_DEFINES = new String[] {
                "define",
                "define-syntax",
                "define-macro",
                "defmacro",
                "define-simple-class",
                "define-class",
                "define-private",
                "define-constant",
                "define-namespace",
                "module-name",
                "module-export",
                "module-static",
                "module-implements",
                "require",
                "define-record-type",
                "define-unit",
                "define-alias",
                "define-base-unit"
    };
    private final static String[] DEFAULT_KEYWORDS = new String[] {
                "#!key", "#!rest", "#!optional"
    };

    private final static String[] DEFAULT_SPECIALS = new String[] {
                "lambda",
                "begin",
                "if",
                "cond",
                "case",
                "do",
                "when",
                "unless",
                "and",
                "or",
                "let",
                "let*",
                "letrec",
                "let-values",
                "let*-values",
                "receive",
                "fluid-let",
                "make",
                "invoke",
                "invoke-static",
                "field",
                "static-field",
                "slot-ref",
                "catch",
                "throw",
                "primitive-throw",
                "try-finally",
                "try-catch",
                "dynamic-wind",
                "future",
                "delay",
                "force",
                "synchronized"
    };
    private final static String[] DEFAULT_MUTATORS = new String[] {
                "set!", "set-car!", "set-cdr!", "vector-set!", "string-set!", "slot-set!"
    };

    private final static String[] DEFAULT_CONSTANTS = new String[] {
                "#!eof", "#!null", "#!void"
    };

    protected Control createContents(Composite parent) {
        Composite composite = new Composite(parent, SWT.NONE);
        GridLayout layout = new GridLayout();
        composite.setLayout(layout);
        layout.marginHeight = 0;
        layout.marginWidth = 0;

        new Label(composite, SWT.NONE).setText("Configure the symbol categories for syntax coloring.");

        TabFolder folder = new TabFolder(composite, SWT.NONE);
        GridData data = new GridData(GridData.FILL_BOTH);
        folder.setLayoutData(data);

        mDefineList = createListControl(folder, "Define", "Defining forms");
        mKeywordList = createListControl(folder, "Special names", "Special names");
        mSpecialList = createListControl(folder, "Special forms", "Special forms");
        mMutatorList = createListControl(folder, "Mutator", "Mutating special forms/functions");
        mConstantList = createListControl(folder, "Constant", "Constants");

        initializeValues();
        return composite;
    }

    private List createListControl(TabFolder folder, String tabName, String toolTip) {
        TabItem item = new TabItem(folder, SWT.NULL);
        item.setText(tabName);
        item.setToolTipText(toolTip);

        Composite composite = new Composite(folder, SWT.NONE);
        GridLayout layout = new GridLayout();
        layout.numColumns = 3;
        composite.setLayout(layout);
        item.setControl(composite);

        final Button deleteButton = new Button(composite, SWT.NONE);
        deleteButton.setText("Delete");
        deleteButton.setToolTipText("Delete the selected symbols");
        deleteButton.setEnabled(false);

        final Button addButton = new Button(composite, SWT.NONE);
        addButton.setText("Add");
        addButton.setToolTipText("Add a new symbol");
        addButton.setEnabled(false);

        final Text text = new Text(composite, SWT.SINGLE | SWT.BORDER);
        GridData data = new GridData(GridData.FILL_HORIZONTAL | GridData.GRAB_HORIZONTAL);
        text.setLayoutData(data);

        final List list = new List(composite, SWT.BORDER | SWT.H_SCROLL | SWT.V_SCROLL | SWT.MULTI);
        data = new GridData(GridData.FILL_BOTH);
        data.horizontalSpan = 3;
        list.setLayoutData(data);
        new ListViewer(list);

        list.addSelectionListener(new SelectionAdapter() {
            public void widgetSelected(SelectionEvent event) {
                deleteButton.setEnabled(list.getSelectionCount() > 0);
            }
        });

        text.addKeyListener(new KeyAdapter() {
            public void keyReleased(KeyEvent event) {
                String symbol = text.getText();
                addButton.setEnabled(SchemeScannerUtilities.isIdentifier(symbol) && (list.indexOf(symbol) == -1));
            }
        });

        addButton.addSelectionListener(new SelectionAdapter() {
            public void widgetSelected(SelectionEvent event) {
                list.add(text.getText());
                text.setText("");
                addButton.setEnabled(false);
            }
        });

        deleteButton.addSelectionListener(new SelectionAdapter() {
            public void widgetSelected(SelectionEvent event) {
                list.remove(list.getSelectionIndices());
            }
        });

        return list;
    }

    public boolean performOk() {
        storeValues();
        return true;
    }

    public void performDefaults() {
        super.performDefaults();

        mDefineList.setItems(DEFAULT_DEFINES);
        mKeywordList.setItems(DEFAULT_KEYWORDS);
        mSpecialList.setItems(DEFAULT_SPECIALS);
        mMutatorList.setItems(DEFAULT_MUTATORS);
        mConstantList.setItems(DEFAULT_CONSTANTS);
    }

    public static void initializeDefaults(IPreferenceStore store) {
        PreferenceUtil.setDefaultKeywords(store, SYNTAX_DEFINE, DEFAULT_DEFINES);
        PreferenceUtil.setDefaultKeywords(store, SYNTAX_KEYWORD, DEFAULT_KEYWORDS);
        PreferenceUtil.setDefaultKeywords(store, SYNTAX_SPECIAL, DEFAULT_SPECIALS);
        PreferenceUtil.setDefaultKeywords(store, SYNTAX_MUTATOR, DEFAULT_MUTATORS);
        PreferenceUtil.setDefaultKeywords(store, SYNTAX_CONSTANT, DEFAULT_CONSTANTS);
    }

    private void initializeValues() {
        IPreferenceStore store = getPreferenceStore();
        mDefineList.setItems(PreferenceUtil.getKeywords(store, SYNTAX_DEFINE));
        mKeywordList.setItems(PreferenceUtil.getKeywords(store, SYNTAX_KEYWORD));
        mSpecialList.setItems(PreferenceUtil.getKeywords(store, SYNTAX_SPECIAL));
        mMutatorList.setItems(PreferenceUtil.getKeywords(store, SYNTAX_MUTATOR));
        mConstantList.setItems(PreferenceUtil.getKeywords(store, SYNTAX_CONSTANT));
    }

    private void storeValues() {
        IPreferenceStore store = getPreferenceStore();
        PreferenceUtil.setKeywords(store, SYNTAX_DEFINE, mDefineList.getItems());
        PreferenceUtil.setKeywords(store, SYNTAX_KEYWORD, mKeywordList.getItems());
        PreferenceUtil.setKeywords(store, SYNTAX_SPECIAL, mSpecialList.getItems());
        PreferenceUtil.setKeywords(store, SYNTAX_MUTATOR, mMutatorList.getItems());
        PreferenceUtil.setKeywords(store, SYNTAX_CONSTANT, mConstantList.getItems());
    }

}