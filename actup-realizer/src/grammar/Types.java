///////////////////////////////////////////////////////////////////////////////
//// Copyright (C) 2003-4 Gunes Erkan and University of Edinburgh (Michael White)
//// 
//// This library is free software; you can redistribute it and/or
//// modify it under the terms of the GNU Lesser General Public
//// License as published by the Free Software Foundation; either
//// version 2.1 of the License, or (at your option) any later version.
//// 
//// This library is distributed in the hope that it will be useful,
//// but WITHOUT ANY WARRANTY; without even the implied warranty of
//// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
//// GNU Lesser General Public License for more details.
//// 
//// You should have received a copy of the GNU Lesser General Public
//// License along with this program; if not, write to the Free Software
//// Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.
////////////////////////////////////////////////////////////////////////////////

package grammar;

import java.io.IOException;
import java.net.URL;
import java.util.ArrayList;
import java.util.BitSet;
import java.util.Collection;
import java.util.Collections;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Set;
import java.util.Stack;

import org.jdom.Document;
import org.jdom.Element;
import org.jdom.JDOMException;
import org.jdom.input.SAXBuilder;

import unify.SimpleType;
import util.GroupMap;

/**
 * Class for constructing and holding the hierarchical simple type maps.
 *
 * @author  Gunes Erkan
 * @author  Michael White
 * @version $Revision: 1.13 $, $Date: 2009/12/21 03:27:18 $
 */
public class Types {
    
    //since simpletypes are immutable, can just provide copies of the lists
    private final List<SimpleType> typeList;
    protected List<SimpleType> copyList() {
    	return new ArrayList<SimpleType>(typeList);
    }
    
    //private int maxTypeIndex = 0;
    public static final String TOP_TYPE = "top";
    public static final String BOT_TYPE = "bottom";
    
    private void addTopType() {
        BitSet bs = new BitSet();
        bs.set(typeList.size());
        SimpleType newtype = new SimpleType(typeList.size(), TOP_TYPE, bs);
        typeList.add(newtype);
    }
    /** Constructor for an empty hierarchy (with just the top type). */
    public Types() {
    	typeList = new ArrayList<SimpleType>();
    	addTopType();
    }

    /**
     * Constructs the type hierarchy from the given URL, for 
     * the given grammar.
     */
    @SuppressWarnings("unchecked")
	public Types(URL url) {
        SAXBuilder builder = new SAXBuilder();
        Document doc = null;
        try {
            doc = builder.build(url);
        }
        catch (JDOMException | IOException exc) {
        	addTopType();
          exc.printStackTrace();
          System.err.println("Some kind of error whiel loading the types from the grammar url");
          System.exit(1);
        }
        List<Element> entries = doc.getRootElement().getChildren();
        this.typeList = readTypes(entries);
    }
    
  


    /** Reads the rules and constructs the nameToType and indexToType maps. */
    private List<SimpleType> readTypes(List<Element> _types) {
        
        GroupMap<String,String> hierarchy = new GroupMap<String,String>(); // map from types to all subtypes
        GroupMap<String,String> parents = new GroupMap<String,String>(); // map from types to parents
        LinkedHashMap<String, Integer> depthMap = new LinkedHashMap<String, Integer>(); // map from types to max depth

        // Construct the initial hierarchy of types without 
        // taking transitive closure.
        // Also store parents.
        for (int i=0; i < _types.size(); i++) {
            Element typeEl = _types.get(i);
            String typeName = typeEl.getAttributeValue("name");
            String _parents = typeEl.getAttributeValue("parents");
            hierarchy.put(typeName, BOT_TYPE);
            if (_parents == null) {
                hierarchy.put(TOP_TYPE, typeName);
                parents.put(typeName, TOP_TYPE);
            }
    		else {
    			String[] parentsArray = _parents.split("\\s+");
    			for (int j = 0; j < parentsArray.length; j++) {
    				hierarchy.put(parentsArray[j], typeName);
                    parents.put(typeName, parentsArray[j]);
    			}
    		}
    	}

        // Compute depth from parents.
        for (String type : parents.keySet()) {
            int depth = computeDepth(type, parents, type);
            depthMap.put(type, depth);
        }

    	// Compute ALL subtypes of each type and insert into the hierarchy.
    	for (String type : hierarchy.keySet()) { 
 		    hierarchy.putAll(type, findAllSubtypes(hierarchy, type));
 		}
        
    	// Assign a unique int to each type in breadth-first order.
    	// Then create the string -> SimpleType map.
    	return createSimpleTypes(hierarchy, depthMap);
    }

    /** Returns the max depth of the given type, checking for cycles. */
    private static int computeDepth(String type, GroupMap<String,String> parents, String startType) {
        if (type.equals(TOP_TYPE)) return 0;
        int maxParentDepth = 0;
        Set<String> parentSet = parents.get(type);
        if (parentSet != null) {
		    for (String parent : parentSet) {
				if (parent.equals(startType)) {
				    throw new RuntimeException("Error, type hierarchy contains cycle from/to: " + startType);
				}
				int parentDepth = computeDepth(parent, parents, startType);
				maxParentDepth = Math.max(maxParentDepth, parentDepth);
			}
        }
        return maxParentDepth + 1;
    }
    
    /** 
     * Computes the list of all sub-types of a given type (key) 
     * in depth-first order. 
     */
    private Collection<String> findAllSubtypes(GroupMap<String,String> hierarchy, String key) {
        ArrayList<String> subs = new ArrayList<String>();
        if (hierarchy.get(key) != null) {
      	    Stack<String> look = new Stack<String>();
      	    for (String type : hierarchy.get(key)) {
      	    	look.push(type);
      	    }
            while (!look.empty()) {
                String new_sub = look.pop();
                subs.add(new_sub);
                if (hierarchy.get(new_sub) != null) {
                    for (String type : hierarchy.get(new_sub)) {
                        look.push(type);
                    }
                }
            }
        }
        return subs;
    }

    /** 
     * Creates the SimpleType objects and constructs the nameToType and indexToType maps. 
     */
    private List<SimpleType> createSimpleTypes(GroupMap<String,String> hierarchy, LinkedHashMap<String, Integer> depthMap) {
        List<SimpleType> typeList = new ArrayList<SimpleType>();
        // find max depth
        int maxDepth = 0;
        Integer[] depths = depthMap.values().toArray(new Integer[depthMap.values().size()]);
        for (int i = 0; i < depths.length; i++) {
            maxDepth = Math.max(maxDepth, depths[i]);
        }

        // add types in order of increasing depth
        ArrayList<String> typesVisited = new ArrayList<String>();
        typesVisited.add(TOP_TYPE);
        String[] types = depthMap.keySet().toArray(new String[depthMap.keySet().size()]);
        ArrayList<String> typesAtSameDepth = new ArrayList<String>();
        for (int i = 1; i <= maxDepth; i++) {
            typesAtSameDepth.clear();
            for (int j = 0; j < types.length; j++) {
                if (depthMap.get(types[j]) == i) {
                    typesAtSameDepth.add((String)types[j]);
                }
            }
            Collections.sort(typesAtSameDepth);
            typesVisited.addAll(typesAtSameDepth);
        }

        // construct the maps
        for (int i=0; i < typesVisited.size(); i++) {
            String typeName = typesVisited.get(i);
            BitSet bitset = new BitSet();
            bitset.set(i);
		    if (hierarchy.get(typeName) != null) {
				for (String type : hierarchy.get(typeName)) {
				    int indexToSet = typesVisited.indexOf(type); 
				    if (indexToSet != -1) {
				    	bitset.set(indexToSet);
				    }
				}
		    }
            SimpleType st = new SimpleType(i, typeName, bitset);
            typeList.add(st);
        }
        
        return typeList;
    }
}
