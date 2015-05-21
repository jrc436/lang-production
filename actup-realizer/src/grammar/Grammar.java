///////////////////////////////////////////////////////////////////////////////
// Copyright (C) 2003-5 University of Edinburgh (Michael White) and Gunes Erkan
// 
// This library is free software; you can redistribute it and/or
// modify it under the terms of the GNU Lesser General Public
// License as published by the Free Software Foundation; either
// version 2.1 of the License, or (at your option) any later version.
// 
// This library is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU Lesser General Public License for more details.
// 
// You should have received a copy of the GNU Lesser General Public
// License along with this program; if not, write to the Free Software
// Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.
//////////////////////////////////////////////////////////////////////////////

package grammar;

import hylo.HyloHelper;

import java.io.BufferedInputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.io.OutputStreamWriter;
import java.net.MalformedURLException;
import java.net.URL;
import java.util.Arrays;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Set;

import javax.xml.parsers.ParserConfigurationException;
import javax.xml.parsers.SAXParser;
import javax.xml.parsers.SAXParserFactory;
import javax.xml.transform.OutputKeys;
import javax.xml.transform.Source;
import javax.xml.transform.Templates;
import javax.xml.transform.Transformer;
import javax.xml.transform.TransformerConfigurationException;
import javax.xml.transform.TransformerException;
import javax.xml.transform.TransformerFactory;
import javax.xml.transform.sax.SAXSource;
import javax.xml.transform.sax.SAXTransformerFactory;
import javax.xml.transform.stream.StreamResult;
import javax.xml.transform.stream.StreamSource;

import lexicon.DefaultTokenizer;
import lexicon.FullWordFactory;
import lexicon.IWordFactory;
import lexicon.LexicalData;
import lexicon.Lexicon;
import lexicon.Tokenizer;

import org.jdom.Document;
import org.jdom.Element;
import org.jdom.JDOMException;
import org.jdom.input.SAXBuilder;
import org.jdom.output.XMLOutputter;
import org.jdom.transform.JDOMResult;
import org.jdom.transform.JDOMSource;
import org.xml.sax.InputSource;
import org.xml.sax.SAXException;
import org.xml.sax.XMLFilter;
import org.xml.sax.XMLReader;

import synsem.LF;
import unify.UnifyControl;

/**
 * A CCG grammar is essentially a lexicon plus a rule group.
 * A grammar may also have sequences of transformations to use in 
 * loading/saving LFs from/to XML.
 *
 * @author  Michael White
 * @author  Gunes Erkan
 * @version $Revision: 1.45 $, $Date: 2010/12/06 02:39:35 $ 
 */

//grammars should generally be read only, meaning that they can be concurrently accessed with no problems. 
//Grammars are additionally /powerful/. It contains a reference to just about everything. In order to counteract the power of grammar,
//objects should only be passed the pieces they need
//there should only ever need to be one grammar, unless different runs wnat to use different styles of wordfactory, etc.
public class Grammar {
	 /** The boundary tones recognized as separate tokens for translation to APML. */
    private static final String[] boundaryTones = { 
        "L", "H", "LL%", "HH%", "LH%", "HL%"
    };
    /** The pitch accents recognized as underscored suffixes for translation to APML. */
    private static final String[] pitchAccents = { 
        "H*", "L*", "L+H*", "L*+H", "H*+L", "H+L*"
    };
    
    private final IWordFactory wf;
    private final RuleGroup rules;
    private final Types types;
    private final TypesData td; //this typesdata should ONLY be used to construct new typesdata and to construct the other things in the grammar
    private final Lexicon l;
    private final Tokenizer t;
    
    public Lexicon getLexicon() {
    	return l;
    }
    public IWordFactory getWordFactory() {
    	return wf;
    }
    public RuleGroup getRuleGroup() {
    	return rules;
    }
    /** NOTE THIS SHOULD VERY RARELY BE USED! It should only be used in preparation, NOT during tasks **/
    public TypesData getGrammarsTypesData() {
    	return this.td;
    }
    public Tokenizer getTokenizer() {
    	return this.t;
    }
    
    
    public LexicalData createNewLexicalData(UnifyControl uc, TypesData td) {
    	return new LexicalData(l, uc, td, wf, t);
    }
    public RuleGroupData createNewRuleGroupData(TypesData td, LexicalData lex, UnifyControl uc) {
    	RuleGroupData retval = new RuleGroupData(wf, td, lex, uc, l, rules, t);
    	lex.setRuleGroupData(retval);
    	return retval;
    }
    public UnifyControl createNewUnifyControl() {
    	return new UnifyControl();
    }
    public TypesData createNewTypesData() {
    	return new TypesData(td);
    }
    
    
    private final Set<String> supertagFeatures = new LinkedHashSet<String>(); //feats to include in supertags
    //strings are immutable, so giving a copy is ok
    public Set<String> copySupertagFeatures() {
    	return new LinkedHashSet<String>(supertagFeatures);
    }
      
    private final URL[] fromXmlTransforms; //xml to LF transforms   
    private final URL[] toXmlTransforms; //LFs to xml transforms
    private final Transformer transformer;
    private final Templates[] fromXmlTemplates;
    private final Templates[] toXmlTemplates;
    private final Transformer apmlTransformer;
   
    
    // name of the grammar
    private final String grammarName;

    // XML factories
    private final SAXParserFactory spf; 
    private final SAXTransformerFactory stf;
    
    private static final Set<String> pitchAccentsSet = new LinkedHashSet<String>(Arrays.asList(pitchAccents));    
    private static final Set<String> boundaryTonesSet = new LinkedHashSet<String>(Arrays.asList(boundaryTones));    
    
    
    /** Loads a grammar from the given URL, with the given flag for whether to ignore rule combos. 
     * @throws IOException **/
	public Grammar(URL url) throws IOException {
    	
    	SAXBuilder builder = new SAXBuilder();
        Document doc;
        try {
            doc = builder.build(url);
        } catch (JDOMException jde) {
            throw (IOException) new IOException().initCause(jde);
        }
        Element root = doc.getRootElement();	// root corresponds to <grammar>
        this.addSuperTagFeatures(root.getChild("supertags"));
        
        grammarName = root.getAttributeValue("name");
        
        wf = new FullWordFactory();
        types = initTypes(root.getChild("types"), url);
        td = new TypesData(this.types);       
        t = initTokenizer(root.getChild("tokenizer"));
        l = setLexicon(root.getChild("lexicon"), root.getChild("morphology"), url, t, wf, td);       
        rules = initRules(root.getChild("rules"), url, l, td, t);
        
        
        this.fromXmlTransforms = initFromXML(root.getChild("LF-from-XML"), url);
        this.toXmlTransforms = initToXML(root.getChild("LF-to-XML"), url);
    	try {
	        spf = SAXParserFactory.newInstance(); 
	        spf.setNamespaceAware(true);
	        
	        stf = (SAXTransformerFactory) TransformerFactory.newInstance();
	        try { // try setting indent at factory level
	            stf.setAttribute("indent-number", new Integer(2));
	        } catch (IllegalArgumentException exc) {} // ignore
			transformer = stf.newTransformer();
            transformer.setOutputProperty(OutputKeys.INDENT, "yes");
            try { // also try setting indent as a xalan property 
                transformer.setOutputProperty("{http://xml.apache.org/xalan}indent-amount", "2");
            } catch (IllegalArgumentException exc) {} // ignore
            InputStream toApmlStr = ClassLoader.getSystemResourceAsStream("grammar/to-apml.xsl");

			apmlTransformer = stf.newTransformer(new StreamSource(toApmlStr));
            apmlTransformer.setOutputProperty(OutputKeys.DOCTYPE_SYSTEM, "apml.dtd");
            
            fromXmlTemplates = new Templates[fromXmlTransforms.length];
            for (int i = 0; i < fromXmlTemplates.length; i++) {
                String turl = fromXmlTransforms[i].toString();
                fromXmlTemplates[i] = stf.newTemplates(new StreamSource(turl));
            }
            
            toXmlTemplates = new Templates[toXmlTransforms.length];
            for (int i = 0; i < toXmlTemplates.length; i++) {
                // File file = new File(toXmlTransforms[i]);
                // toXmlTemplates[i] = stf.newTemplates(new StreamSource(file));
                String turl = toXmlTransforms[i].toString();
                toXmlTemplates[i] = stf.newTemplates(new StreamSource(turl));
            }
    	}
    	catch (TransformerConfigurationException tce) {
    		tce.printStackTrace();
    		throw new IOException();
    	}
    }
	private static RuleGroup initRules(Element rulesElt, URL url, Lexicon l, TypesData td, Tokenizer t) {
		try {
        	URL rulesUrl = new URL(url, rulesElt.getAttributeValue("file"));
	        String combosfile = rulesElt.getAttributeValue("combosfile");
	        String dynamicCombos = rulesElt.getAttributeValue("dynamic-combos");
	        if (combosfile != null && dynamicCombos != null) {
	        	return new RuleGroup(rulesUrl, new URL(url, combosfile), l, td, Boolean.parseBoolean(dynamicCombos), t);
	        }
	        else if (dynamicCombos != null) {
	        	return new RuleGroup(rulesUrl, l, td, Boolean.parseBoolean(dynamicCombos), t);
	        }
	        else if (combosfile != null) {
	        	return new RuleGroup(rulesUrl, l, td, new URL(url, combosfile), t);
	        }
	        else {
	        	return new RuleGroup(rulesUrl, l, td, t);
	        }
        }	
		catch (IOException e) {
			e.printStackTrace();
			System.exit(1);
		}
		return null;
       
	}
	private static Types initTypes(Element typesElt, URL url) {
		URL typesUrl = null;
		Types types = null;
        if (typesElt != null) {
            try {
				typesUrl = new URL(url, typesElt.getAttributeValue("file"));
			} 
            catch (MalformedURLException e) {
				e.printStackTrace();
				System.exit(1);
			}
        }
        types = typesUrl != null ? new Types(typesUrl) : new Types();
        return types;
	}
	
    private static Lexicon setLexicon(Element lexiconElt, Element morphElt, URL url, Tokenizer token, IWordFactory wf, TypesData td) {
    	boolean openlex = "true".equals(lexiconElt.getAttributeValue("openlex")); 
    	 URL lexiconUrl = null;
    	 URL morphUrl = null;
         try {
			lexiconUrl = new URL(url, lexiconElt.getAttributeValue("file"));
			morphUrl = new URL(url, morphElt.getAttributeValue("file"));
		} catch (MalformedURLException e) {
			e.printStackTrace();
			System.err.println("Error loading lexicon or morphology!!");
			System.exit(1);
		} 
        try {
			Lexicon l = new Lexicon(td, token, wf);
			l.init(openlex, lexiconUrl, morphUrl);
			return l;
		} 
        catch (IOException e) {
			e.printStackTrace();
			System.err.println("Error creating Lexicon");
			System.exit(1);
		}
        return null;
    }
    private static Tokenizer initTokenizer(Element tokenizerElt) {
    	Tokenizer tokenizer = null;
    	if (tokenizerElt == null) {
    		return new DefaultTokenizer(new String[0]);
    	}
    	String repClasses = tokenizerElt.getAttributeValue("replacement-sem-classes");
        String[] semClasses = repClasses == null ? new String[0] : repClasses.split("\\s+");
		String tokenClass = tokenizerElt.getAttributeValue("classname");	
		try {
			tokenizer = tokenClass == null ? new DefaultTokenizer(semClasses) : (Tokenizer) Class.forName(tokenClass).newInstance();
		} 
		catch (InstantiationException | IllegalAccessException | ClassNotFoundException e) {
			e.printStackTrace();
			System.err.println(tokenClass+" is not a valid classname, apparently");
			System.exit(1);
		}       
    	return tokenizer;
    }
    
    /**
     * Returns a file url string relative to the user's current directory 
     * for the given filename.
     */
    public static String convertToFileUrl(String filename) {
        try {
            return new File(filename).toURI().toURL().toString();
        }
        catch (java.net.MalformedURLException exc) {
            throw (RuntimeException) new RuntimeException().initCause(exc);
        }
        // return "file:"+System.getProperty("user.dir")+"/"+filename;
    }   
    
    // does setup for LF from XML transformation, and returns a SAXSource for the given input stream
    // nb: need a new filter chain one for each use (perhaps due to an underyling bug)
    private SAXSource fromXmlSetup(InputStream istream) throws IOException {
        try {
            // initialize transformer
            // set up initial reader
            SAXParser parser = spf.newSAXParser();
            XMLReader reader = parser.getXMLReader();
            // set up chain of filters
            XMLFilter[] filters = new XMLFilter[fromXmlTransforms.length];
            for (int i = 0; i < filters.length; i++) {
                // create filter
                filters[i] = stf.newXMLFilter(fromXmlTemplates[i]);
                // set parent
                if (i == 0) { filters[0].setParent(reader); }
                else { filters[i].setParent(filters[i-1]); }
            }
            // set final reader/filter
            XMLReader finalReader = (filters.length == 0) ? reader : filters[filters.length-1];
            // set up and return LF from XML SAX source with final reader/filter
            return new SAXSource(finalReader, new InputSource(istream));
        } catch (ParserConfigurationException pce) {
            throw (IOException) new IOException().initCause(pce);
        } catch (SAXException se) {
            throw (IOException) new IOException().initCause(se);
        } catch (TransformerConfigurationException tce) {
            throw (IOException) new IOException().initCause(tce);
        }
    }
    
    /**
     * Loads a document from the XML in the given input stream, 
     * applying the configured from-XML transformations.
     */
    public synchronized Document loadFromXml(InputStream istream) throws IOException {
        try {
            // do setup and get source
            Source source = fromXmlSetup(istream);
            // do transformation
            JDOMResult result = new JDOMResult();
            transformer.transform(source, result);
            // return result doc
            return result.getDocument();
        } catch (TransformerException exc) { 
            throw (IOException) new IOException().initCause(exc);
        }
    }
    
    /**
     * Loads a document from the XML file with the given filename, 
     * applying the configured from-XML transformations.
     */
    public synchronized Document loadFromXml(String filename) throws IOException {
        BufferedInputStream bis = new BufferedInputStream(new FileInputStream(filename));
        Document retval = loadFromXml(bis);
        bis.close();
        return retval;
    }
    

    // does setup for LF to XML transformation, and returns a SAXSource for the given source
    // nb: need a new filter chain one for each use (perhaps due to an underyling bug)
    private SAXSource toXmlSetup(Source source) throws IOException {
        try {
            // set up initial reader
            SAXParser parser = spf.newSAXParser();
            XMLReader reader = parser.getXMLReader();
            // set up chain of filters
            XMLFilter[] filters = new XMLFilter[toXmlTransforms.length];
            for (int i = 0; i < filters.length; i++) {
                // create filter
                filters[i] = stf.newXMLFilter(toXmlTemplates[i]);
                // set parent
                if (i == 0) { filters[0].setParent(reader); }
                else { filters[i].setParent(filters[i-1]); }
            }
            // set final reader/filter
            XMLReader finalReader = (filters.length == 0) ? reader : filters[filters.length-1];
            // set up and return LF to XML SAX source with final reader/filter
            return new SAXSource(finalReader, SAXSource.sourceToInputSource(source));
        } catch (ParserConfigurationException pce) {
            throw (IOException) new IOException().initCause(pce);
        } catch (SAXException se) {
            throw (IOException) new IOException().initCause(se);
        } catch (TransformerConfigurationException tce) {
            throw (IOException) new IOException().initCause(tce);
        }
    }

    /**
     * Saves the given LF with the given target string to an XML file 
     * with the given filename, applying the configured to-XML
     * transformations.
     */
    public synchronized void saveToXml(LF lf, String target, String filename) throws IOException { 
        // ensure dirs exist for filename
        File file = new File(filename);
        File parent = file.getParentFile();
        if (parent != null && !parent.exists()) { parent.mkdirs(); }
        FileOutputStream out = new FileOutputStream(file); 
        saveToXml(lf, target, out);
        out.close();
    }

    /**
     * Saves the given LF with the given target string as XML to the 
     * given output stream, applying the configured to-XML
     * transformations.
     */
    public synchronized void saveToXml(LF lf, String target, OutputStream out) throws IOException { 
        // make doc with XML for LF and target
        Document doc = new Document();
        Element root = new Element("xml");
        doc.setRootElement(root);
        root.addContent(HyloHelper.toXml(lf));
        Element targetElt = new Element("target");
        targetElt.addContent(target);
        root.addContent(targetElt);

        // write transformed doc to file
        try {
            // do setup and get source
            Source source = toXmlSetup(new JDOMSource(doc));
            // do transformation
            transformer.transform(source, new StreamResult(new OutputStreamWriter(out)));
        } catch (TransformerException exc) { 
            throw (IOException) new IOException().initCause(exc);
        }
    }
    
    /**
     * Convenience method to serialize XML.
     */
    public synchronized void serializeXml(Document doc, OutputStream out) throws IOException {
        try {
            JDOMResult result = new JDOMResult(); // as suggested by Amy Isard, for better java/xml version compatibility
            transformer.transform(new JDOMSource(doc), result);
            XMLOutputter outputter = new XMLOutputter();
            outputter.output(result.getDocument(), new OutputStreamWriter(out)); // end of A.I. suggestion
        } catch (TransformerException exc) { 
            throw (IOException) new IOException().initCause(exc);
        }
    }

    
    /** 
     * Makes an element for the given LF, applying the configured to-XML transformations.
     */
    public synchronized Element makeLfElt(LF lf) throws IOException { 
        // make doc with LF in it
        Document lfDoc = new Document();
        lfDoc.setRootElement(HyloHelper.toXml(lf));
        // apply to-XML transformations
        try {
            // do setup and get source
            Source source = toXmlSetup(new JDOMSource(lfDoc));
            // do transformation and get resulting doc
            JDOMResult result = new JDOMResult();
            transformer.transform(source, result);
            lfDoc = result.getDocument();
        } catch (TransformerException exc) { 
            throw (IOException) new IOException().initCause(exc);
        }
        return lfDoc.detachRootElement();
    }

    
    /** 
     * Returns whether the given string is a recognized pitch accent.
     */
    public static boolean isPitchAccent(String s) {
        return pitchAccentsSet.contains(s);
    }
    
    /** 
     * Returns whether the given string is a recognized boundary tone. 
     */
    public static boolean isBoundaryTone(String s) {
        return boundaryTonesSet.contains(s);
    }

	/**
	* Returns the name of the loaded grammar. Null if no name given.
	*/
	public final String getName() {
		return grammarName;
	}
	private void addSuperTagFeatures(Element supertagsElt) {
		if (supertagsElt != null) {
            String feats = supertagsElt.getAttributeValue("feats");
            if (feats != null) {
                String[] names = feats.split("\\s+");
                for (int i = 0; i < names.length; i++) {
                    supertagFeatures.add(names[i]);
                }
            }
        }
        if (supertagFeatures.isEmpty()) {
            // default is "form" and "lex"
            supertagFeatures.add("form"); 
            supertagFeatures.add("lex"); 
        }
	}
    @SuppressWarnings("unchecked")
	private URL[] initFromXML(Element fromXmlElt, URL url) {
        URL[] fromXmlTransforms = null;
    	if (fromXmlElt != null) {
            List<Element> children = fromXmlElt.getChildren();
            fromXmlTransforms = new URL[children.size()];
            for (int i = 0; i < children.size(); i++) {
                Element transformElt = (Element) children.get(i);
                try {
					fromXmlTransforms[i] = new URL(url, transformElt.getAttributeValue("file"));
				} catch (MalformedURLException e) {
					e.printStackTrace();
					System.exit(1);
				}
            }
        }
    	else {
            fromXmlTransforms = new URL[0];
        }
    	return fromXmlTransforms;
    }
    @SuppressWarnings("unchecked")
	private URL[] initToXML(Element toXmlElt, URL url) {
        URL[] toXmlTransforms = null;
        if (toXmlElt != null) {
            List<Element> children = toXmlElt.getChildren();
            toXmlTransforms = new URL[children.size()];
            for (int i = 0; i < children.size(); i++) {
                Element transformElt = (Element) children.get(i);
                try {
					toXmlTransforms[i] = new URL(url, transformElt.getAttributeValue("file"));
				} 
                catch (MalformedURLException e) {
					e.printStackTrace();
					System.exit(1);
				}
            }
        } 
        else {
            toXmlTransforms = new URL[0];
        }
        return toXmlTransforms;
    }
	
}

