package org.search.pretreat.core;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.search.pretreat.core.commander.FoodCommander;
import org.search.pretreat.core.model.request.PretreatRequest;
import org.search.pretreat.core.model.response.Instruction;
import org.search.pretreat.core.model.response.QueryUnderstand;

public class IntentCore {

	public static void main(String[] args) {
		// TODO Auto-generated method stub

	}
	
	public QueryUnderstand getSearchInstruction(PretreatRequest request) {
		long startTime = System.currentTimeMillis();
		List<Instruction> instructions = new ArrayList<>();
		boolean isFoodQuery = false;
		boolean isRetailQuery = false;
		boolean isVagueQuery = false;	// todo
		boolean isProhibitedQuery = false;
		boolean isExemptFromFilter = false;
		Map<Integer, Double> categoryIntent = new HashMap<>();
		List<String> relevantQueries = new ArrayList<>();
		String stickToTopShopName = null;
		Map<String, String> BDILabelMap = new HashMap<>();
		
		isFoodQuery = FoodCommander.isFoodQuery(request.getQuery());
		
		
		return null;
	}

}
