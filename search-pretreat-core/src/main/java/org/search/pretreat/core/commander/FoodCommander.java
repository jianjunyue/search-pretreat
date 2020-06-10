package org.search.pretreat.core.commander;

import java.io.BufferedReader;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;
import org.search.pretreat.nlp.train.models.DisplayStylePredictor;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class FoodCommander {
	private static Logger logger = LoggerFactory.getLogger(FoodCommander.class);

	private static Set<String> shopStyleWhiteList = new HashSet<>();
	private static Set<String> foodStyleWhiteList = new HashSet<>();

	private static Map<String, String> pinyin2hanziCache = new HashMap<>();;

	public static boolean isFoodQuery(String query) {
		if (query == null) {
			return false;
		}
		query = query.replaceAll(" ", "").toLowerCase();
		if (foodStyleWhiteList.contains(query)) {
			return true;
		} else if (shopStyleWhiteList.contains(query)) {
			return false;
		}
		// 如果是一个能找到汉字映射的拼音，那就作为店铺维度
		if (pinyin2hanziCache.containsKey(query)) {
			return true;
		}
		return DisplayStylePredictor.getInstance().isFoodDimensionQuery(query);
	}

	static {
		String line;
		InputStream in = Thread.currentThread().getContextClassLoader().getResourceAsStream("pinyin_2_hanzi.tsv");
		BufferedReader reader = new BufferedReader(new InputStreamReader(in));
		try {
			while ((line = reader.readLine()) != null) {
				String[] tokens = line.trim().split("\t");
				if (tokens.length > 2) {
					String pinyin = tokens[0].trim();
					String[] subTokens = tokens[1].trim().split(":");
					if (subTokens.length == 3) {
						pinyin2hanziCache.put(pinyin, subTokens[0]);
					}
				}
			}
			in.close();
			reader.close();
		} catch (Exception e) {
		}
		logger.info("pinyin2hanziCache: " + pinyin2hanziCache.size());

	}

}
