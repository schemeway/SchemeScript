/*
 * Copyright (c) 2004 Nu Echo Inc.
 * 
 * This is free software. For terms and warranty disclaimer, see ./COPYING
 */
package org.schemeway.plugins.schemescript.preferences;

import java.util.regex.*;

import org.eclipse.jface.preference.*;
import org.eclipse.jface.viewers.*;
import org.eclipse.swt.*;
import org.eclipse.swt.events.*;
import org.eclipse.swt.layout.*;
import org.eclipse.swt.widgets.*;
import org.schemeway.plugins.schemescript.*;
import org.schemeway.plugins.schemescript.parser.*;

public class SyntaxPreferences extends SchemePreferencePage {
    public final static String PREFIX = SchemeScriptPlugin.PLUGIN_NS + ".syntax.";
    public final static String SYNTAX_KEYWORD = PREFIX + "keywords";
    public final static String SYNTAX_DEFINE = PREFIX + "define";
    public final static String SYNTAX_SPECIAL = PREFIX + "special";
    public final static String SYNTAX_MUTATOR = PREFIX + "mutator";
    public final static String SYNTAX_CONSTANT = PREFIX + "constant";
    public final static String SYNTAX_KEYWORD_RE = PREFIX + "keywords-re";
    public final static String SYNTAX_DEFINE_RE = PREFIX + "define-re";
    public final static String SYNTAX_SPECIAL_RE = PREFIX + "special-re";
    public final static String SYNTAX_MUTATOR_RE = PREFIX + "mutator-re";
    public final static String SYNTAX_CONSTANT_RE = PREFIX + "constant-re";

    private static class SyntaxCategoryWidgets {
    	public List listbox;
    	public Text reTextbox;

    	public SyntaxCategoryWidgets(List list, Text textbox) {
			super();
			this.listbox = list;
			reTextbox = textbox;
		}
    }
    
    private SyntaxCategoryWidgets mDefineWidgets;
    private SyntaxCategoryWidgets mKeywordWidgets;
    private SyntaxCategoryWidgets mSpecialWidgets;
    private SyntaxCategoryWidgets mMutatorWidgets;
    private SyntaxCategoryWidgets mConstantWidgets;

    private final static String[] DEFAULT_DEFINES = new String[] {
                "define",
                "define*",
                "define-syntax",
                "define-macro",
                "define-macro*",
                "define-syntax*",
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
                "synchronized",
                "library",
                "import",
                "export",
                "package*",
                "include*"
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
        data.heightHint = 400;
        folder.setLayoutData(data);

        mDefineWidgets = createListControl(folder, "Define", "Defining forms");
        mKeywordWidgets = createListControl(folder, "Special names", "Special names");
        mSpecialWidgets = createListControl(folder, "Special forms", "Special forms");
        mMutatorWidgets = createListControl(folder, "Mutator", "Mutating special forms/functions");
        mConstantWidgets = createListControl(folder, "Constant", "Constants");

        initializeValues();
        return composite;
    }

    private SyntaxCategoryWidgets createListControl(TabFolder folder, String tabName, String toolTip) {
    	GridData data;
    	
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
        data = new GridData(GridData.HORIZONTAL_ALIGN_FILL);
        data.horizontalSpan = 1;
        deleteButton.setLayoutData(data);

        final Button addButton = new Button(composite, SWT.NONE);
        addButton.setText("Add");
        addButton.setToolTipText("Add a new symbol");
        addButton.setEnabled(false);
        data = new GridData(GridData.HORIZONTAL_ALIGN_FILL);
        data.horizontalSpan = 1;
        addButton.setLayoutData(data);

        final Text text = new Text(composite, SWT.SINGLE | SWT.BORDER);
        data = new GridData(GridData.FILL_HORIZONTAL | GridData.GRAB_HORIZONTAL);
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

        Label reLabel = new Label(composite, SWT.NONE);
        reLabel.setText("Regular expression:");
        data = new GridData(GridData.BEGINNING);
        data.horizontalSpan = 2;
        reLabel.setLayoutData(data);
        
        final Text reText = new Text(composite, SWT.BORDER);
        data = new GridData(GridData.FILL_HORIZONTAL);
        reText.setLayoutData(data);
        
        reText.addKeyListener(new KeyAdapter() {
        	public void keyReleased(KeyEvent e) {
        		String eventText = reText.getText();
        		try {
        			Pattern.compile(eventText);
        			setValid(true);
        			setErrorMessage(null);
        		}
        		catch (PatternSyntaxException exception) {
        			setValid(false);
        			setErrorMessage("Invalid regular expression");
        		}
        	}
        });
        
        return new SyntaxCategoryWidgets(list, reText);
    }

    protected void doPerformDefaults() {
        mDefineWidgets.listbox.setItems(DEFAULT_DEFINES);
        mDefineWidgets.reTextbox.setText("");
        mKeywordWidgets.listbox.setItems(DEFAULT_KEYWORDS);
        mKeywordWidgets.reTextbox.setText("");
        mSpecialWidgets.listbox.setItems(DEFAULT_SPECIALS);
        mSpecialWidgets.reTextbox.setText("");
        mMutatorWidgets.listbox.setItems(DEFAULT_MUTATORS);
        mMutatorWidgets.reTextbox.setText("");
        mConstantWidgets.listbox.setItems(DEFAULT_CONSTANTS);
        mConstantWidgets.reTextbox.setText("");
    }

    public static void initializeDefaults(IPreferenceStore store) {
        PreferenceUtil.setDefaultKeywords(store, SYNTAX_DEFINE, DEFAULT_DEFINES);
        store.setDefault(SYNTAX_DEFINE_RE, "");
        PreferenceUtil.setDefaultKeywords(store, SYNTAX_KEYWORD, DEFAULT_KEYWORDS);
        store.setDefault(SYNTAX_KEYWORD_RE, "");
        PreferenceUtil.setDefaultKeywords(store, SYNTAX_SPECIAL, DEFAULT_SPECIALS);
        store.setDefault(SYNTAX_SPECIAL_RE, "");
        PreferenceUtil.setDefaultKeywords(store, SYNTAX_MUTATOR, DEFAULT_MUTATORS);
        store.setDefault(SYNTAX_MUTATOR_RE, "");
        PreferenceUtil.setDefaultKeywords(store, SYNTAX_CONSTANT, DEFAULT_CONSTANTS);
        store.setDefault(SYNTAX_CONSTANT_RE, "");
    }

    protected void initializeValues() {
        IPreferenceStore store = getPreferenceStore();
        mDefineWidgets.listbox.setItems(PreferenceUtil.getKeywords(store, SYNTAX_DEFINE));
        mDefineWidgets.reTextbox.setText(store.getString(SYNTAX_DEFINE_RE));
        mKeywordWidgets.listbox.setItems(PreferenceUtil.getKeywords(store, SYNTAX_KEYWORD));
        mKeywordWidgets.reTextbox.setText(store.getString(SYNTAX_KEYWORD_RE));
        mSpecialWidgets.listbox.setItems(PreferenceUtil.getKeywords(store, SYNTAX_SPECIAL));
        mSpecialWidgets.reTextbox.setText(store.getString(SYNTAX_SPECIAL_RE));
        mMutatorWidgets.listbox.setItems(PreferenceUtil.getKeywords(store, SYNTAX_MUTATOR));
        mMutatorWidgets.reTextbox.setText(store.getString(SYNTAX_MUTATOR_RE));
        mConstantWidgets.listbox.setItems(PreferenceUtil.getKeywords(store, SYNTAX_CONSTANT));
        mConstantWidgets.reTextbox.setText(store.getString(SYNTAX_CONSTANT_RE));
        
    }

    protected void storeValues() {
        IPreferenceStore store = getPreferenceStore();
        PreferenceUtil.setKeywords(store, SYNTAX_DEFINE, mDefineWidgets.listbox.getItems());
        store.setValue(SYNTAX_DEFINE_RE, mDefineWidgets.reTextbox.getText());
        PreferenceUtil.setKeywords(store, SYNTAX_KEYWORD, mKeywordWidgets.listbox.getItems());
        store.setValue(SYNTAX_KEYWORD_RE, mKeywordWidgets.reTextbox.getText());
        PreferenceUtil.setKeywords(store, SYNTAX_SPECIAL, mSpecialWidgets.listbox.getItems());
        store.setValue(SYNTAX_SPECIAL_RE, mSpecialWidgets.reTextbox.getText());
        PreferenceUtil.setKeywords(store, SYNTAX_MUTATOR, mMutatorWidgets.listbox.getItems());
        store.setValue(SYNTAX_MUTATOR_RE, mMutatorWidgets.reTextbox.getText());
        PreferenceUtil.setKeywords(store, SYNTAX_CONSTANT, mConstantWidgets.listbox.getItems());
        store.setValue(SYNTAX_CONSTANT_RE, mConstantWidgets.reTextbox.getText());
    }

}