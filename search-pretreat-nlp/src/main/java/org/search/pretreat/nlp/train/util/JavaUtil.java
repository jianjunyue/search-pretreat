package org.search.pretreat.nlp.train.util;

import java.text.DateFormat;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Calendar;
import java.util.Date;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Set;
import java.util.TreeMap;
import java.util.stream.Collectors;
import org.apache.commons.lang3.StringUtils;
import org.apache.commons.lang3.tuple.ImmutablePair;

public class JavaUtil {
	private static final String NOT_FOOD_START = "NOTFOODSTART";

	private static final String NOT_FOOD_END = "NOTFOODEND";

	public static final String LENGTH_FEATURE = "SEQUENCE_LENGTH_";

	public static final String PREFIX = "gram_";

	public static String processQuery(String query) {
		return query.replaceAll("(\\(|（).*(\\)|）|$)", "").trim().toLowerCase();
	}

	public static boolean checkBracket(String query) {
		if (query == null)
			return false;
		if (query.contains("(") || query.contains("（") || query.contains(")") || query.contains("）")) {
			return true;
		}
		return false;
	}

	public static String processTimestamp(long timeStamp) {
		if (timeStamp == -1L)
			return "NULL";
		Date time = new Date(timeStamp);
		Calendar calendar = Calendar.getInstance();
		calendar.setTime(time);
		int dayOfWeek = calendar.get(7);
		DateFormat df = new SimpleDateFormat("HH:mm:ss");
		return dayOfWeek + "-" + df.format(time).substring(0, 4);
	}

	public static String[] getGrams(String query, int length) {
		List<String> buf = new ArrayList<>();
		for (int i = 0; i < query.length(); i++) {
			for (int j = i + 1; j <= Math.min(i + length, query.length()); j++) {
				String gram = query.substring(i, j).trim();
				if (gram.length() > 0)
					buf.add(gram);
			}
		}
		String[] grams = new String[buf.size()];
		buf.toArray(grams);
		return grams;
	}

	public static Map<Integer, Double> getVector(String regQuery, int length, Map<String, Integer> featureMap) {
		Map<Integer, Double> vec = new TreeMap<>();
		for (String gram : getGrams(regQuery, length)) {
			if (featureMap.containsKey("gram_" + gram)) {
				int featureId = ((Integer) featureMap.get("gram_" + gram)).intValue();
				double value = ((Double) vec.getOrDefault(Integer.valueOf(featureId), Double.valueOf(0.0D)))
						.doubleValue();
				vec.put(Integer.valueOf(featureId), Double.valueOf(value + 1.0D));
			}
		}
		if (featureMap.containsKey("len_" + regQuery.length())) {
			int featureId = ((Integer) featureMap.get("len_" + regQuery.length())).intValue();
			vec.put(Integer.valueOf(featureId), Double.valueOf(1.0D));
		}
		return vec;
	}

	public static boolean notFoodStart(String query, Set foodSet) {
		if (StringUtils.isBlank(query) || Objects.isNull(foodSet))
			return Boolean.FALSE.booleanValue();
		for (int i = 1; i < query.length() + 1; i++) {
			String gram = query.substring(0, i).trim();
			if (foodSet.contains(gram))
				return Boolean.FALSE.booleanValue();
		}
		return Boolean.TRUE.booleanValue();
	}

	public static boolean notFoodEnd(String query, Set foodSet) {
		if (StringUtils.isBlank(query) || Objects.isNull(foodSet))
			return Boolean.FALSE.booleanValue();
		int len = query.length();
		for (int i = len - 1; i >= 0; i--) {
			String gram = query.substring(i, len).trim();
			if (foodSet.contains(gram))
				return Boolean.FALSE.booleanValue();
		}
		return Boolean.TRUE.booleanValue();
	}

	public static ImmutablePair<String, String> ngramFoodAndNotFoodCnt(List<String> ngrams, Set<String> ngramFoodSet) {
		String ngramFoodCNT = CountEnum.FOOD_CNT_0.getName();
		String ngramNotFoodCNT = CountEnum.NOT_FOOD_CNT_0.getName();
		String comparedCnt = ngramNotFoodCNT + "/" + ngramFoodCNT;
		if (Objects.isNull(ngrams) || ngrams.isEmpty() || Objects.isNull(ngramFoodSet) || ngramFoodSet.isEmpty())
			return new ImmutablePair(ngramNotFoodCNT, comparedCnt);
		Set<String> ngramSet = (Set<String>) ngrams.stream()
				.map(gram -> gram.replace("^", "").replace("$", "").replace("~", ""))
				.filter(word -> (word.length() >= 2)).collect(Collectors.toSet());
		int foodCnt = 0;
		int notFoodCnt = 0;
		for (String gram : ngramSet) {
			if (ngramFoodSet.contains(gram)) {
				foodCnt++;
				continue;
			}
			notFoodCnt++;
		}
		if (foodCnt > 10)
			foodCnt = 10;
		if (notFoodCnt > 10)
			notFoodCnt = 10;
		ngramFoodCNT = CountEnum.getNameByCount(foodCnt, 1);
		ngramNotFoodCNT = CountEnum.getNameByCount(notFoodCnt, 2);
		comparedCnt = ngramNotFoodCNT + "/" + ngramFoodCNT;
		return new ImmutablePair(ngramNotFoodCNT, comparedCnt);
	}

	public static String[] getSpecialNGrams(String query, int length, Set<String> foodSet,
			Map<String, Integer> ngramFoodMap) {
		List<String> buf = new ArrayList<>();
		for (int i = 0; i < query.length(); i++) {
			for (int j = i + 1; j <= Math.min(i + length, query.length()); j++) {
				String gram = query.substring(i, j).trim();
				String prefixGram = "";
				String suffixGram = "";
				String allMatch = "";
				if (query.equals(gram)) {
					allMatch = "~" + gram;
					prefixGram = "^" + gram;
					suffixGram = gram + "$";
				} else if (query.startsWith(gram)) {
					prefixGram = "^" + gram.trim();
				} else if (query.endsWith(gram)) {
					suffixGram = gram.trim() + "$";
				}
				if (gram.length() > 0)
					buf.add(gram);
				if (prefixGram.length() > 0)
					buf.add(prefixGram);
				if (suffixGram.length() > 0)
					buf.add(suffixGram);
				if (allMatch.length() > 0)
					buf.add(allMatch);
			}
		}
		ImmutablePair<String, String> countImmutablePair = ngramFoodAndNotFoodCnt(buf, ngramFoodMap.keySet());
		String ngramNotFoodCNT = (String) countImmutablePair.getLeft();
		String comparedCnt = (String) countImmutablePair.getRight();
		buf.add(comparedCnt);
		buf.add(ngramNotFoodCNT);
		Boolean notStartflag = Boolean.valueOf(notFoodStart(query, foodSet));
		if (notStartflag != null && notStartflag.booleanValue())
			buf.add("NOTFOODSTART");
		Boolean notEndflag = Boolean.valueOf(notFoodEnd(query, foodSet));
		if (notEndflag != null && notEndflag.booleanValue())
			buf.add("NOTFOODEND");
		buf.add(getAddedFeature(query, splits, ngramFoodMap));
		buf.add(getAddedFeature2(query, splits2, ngramFoodMap, foodSet));
		buf.add(getAddedFeature3(query, splits3, ngramFoodMap, foodSet));
		String lengthFeature = "SEQUENCE_LENGTH_" + String.valueOf(query.length());
		buf.add(lengthFeature);
		String[] grams = new String[buf.size()];
		buf.toArray(grams);
		return grams;
	}

	public static String[] getSpecialNGrams(String query, int length, Set<String> foodSet) {
		List<String> buf = new ArrayList<>();
		for (int i = 0; i < query.length(); i++) {
			for (int j = i + 1; j <= Math.min(i + length, query.length()); j++) {
				String gram = query.substring(i, j).trim();
				String prefixGram = "";
				String suffixGram = "";
				String allMatch = "";
				if (query.equals(gram)) {
					allMatch = "~" + gram;
					prefixGram = "^" + gram;
					suffixGram = gram + "$";
				} else if (query.startsWith(gram)) {
					prefixGram = "^" + gram.trim();
				} else if (query.endsWith(gram)) {
					suffixGram = gram.trim() + "$";
				}
				if (gram.length() > 0)
					buf.add(gram);
				if (prefixGram.length() > 0)
					buf.add(prefixGram);
				if (suffixGram.length() > 0)
					buf.add(suffixGram);
				if (allMatch.length() > 0)
					buf.add(allMatch);
			}
		}
		Boolean notStartflag = Boolean.valueOf(notFoodStart(query, foodSet));
		if (notStartflag != null && notStartflag.booleanValue())
			buf.add("NOTFOODSTART");
		Boolean notEndflag = Boolean.valueOf(notFoodEnd(query, foodSet));
		if (notEndflag != null && notEndflag.booleanValue())
			buf.add("NOTFOODEND");
		String[] grams = new String[buf.size()];
		buf.toArray(grams);
		return grams;
	}

	public static String[] getSpecialNGrams(String query, int length) {
		List<String> buf = new ArrayList<>();
		for (int i = 0; i < query.length(); i++) {
			for (int j = i + 1; j <= Math.min(i + length, query.length()); j++) {
				String gram = query.substring(i, j).trim();
				String prefixGram = "";
				String suffixGram = "";
				String allMatch = "";
				if (query.equals(gram)) {
					allMatch = "~" + gram;
					prefixGram = "^" + gram;
					suffixGram = gram + "$";
				} else if (query.startsWith(gram)) {
					prefixGram = "^" + gram.trim();
				} else if (query.endsWith(gram)) {
					suffixGram = gram.trim() + "$";
				}
				if (gram.length() > 0)
					buf.add(gram);
				if (prefixGram.length() > 0)
					buf.add(prefixGram);
				if (suffixGram.length() > 0)
					buf.add(suffixGram);
				if (allMatch.length() > 0)
					buf.add(allMatch);
			}
		}
		String[] grams = new String[buf.size()];
		buf.toArray(grams);
		return grams;
	}

	public static Map<Integer, Double> getVector(String regQuery, int length, Map<String, Integer> featureMap,
			Set<String> foodSet, Map<String, Integer> ngramFoodMap) {
		Map<Integer, Double> vec = new TreeMap<>();
		for (String gram : getSpecialNGrams(regQuery, length, foodSet, ngramFoodMap)) {
			if (featureMap.containsKey("gram_" + gram)) {
				int featureId = ((Integer) featureMap.get("gram_" + gram)).intValue();
				double value = vec.containsKey(Integer.valueOf(featureId))
						? ((Double) vec.get(Integer.valueOf(featureId))).doubleValue()
						: 0.0D;
				vec.put(Integer.valueOf(featureId), Double.valueOf(value + 1.0D));
			}
		}
		return vec;
	}

	public enum CountEnum {
		FOOD_CNT_0(0, "FOODCNT0", 1), FOOD_CNT_1(1, "FOODCNT1", 1), FOOD_CNT_2(2, "FOODCNT2", 1),
		FOOD_CNT_3(3, "FOODCNT3", 1), FOOD_CNT_4(4, "FOODCNT4", 1), FOOD_CNT_5(5, "FOODCNT5", 1),
		FOOD_CNT_6(6, "FOODCNT6", 1), FOOD_CNT_7(7, "FOODCNT7", 1), FOOD_CNT_8(8, "FOODCNT8", 1),
		FOOD_CNT_9(9, "FOODCNT9", 1), FOOD_CNT_10(10, "FOODCNT10", 1), NOT_FOOD_CNT_0(0, "NOTFOODCNT0", 2),
		NOT_FOOD_CNT_1(1, "NOTFOODCNT1", 2), NOT_FOOD_CNT_2(2, "NOTFOODCNT2", 2), NOT_FOOD_CNT_3(3, "NOTFOODCNT3", 2),
		NOT_FOOD_CNT_4(4, "NOTFOODCNT4", 2), NOT_FOOD_CNT_5(5, "NOTFOODCNT5", 2), NOT_FOOD_CNT_6(6, "NOTFOODCNT6", 2),
		NOT_FOOD_CNT_7(7, "NOTFOODCNT7", 2), NOT_FOOD_CNT_8(8, "NOTFOODCNT8", 2), NOT_FOOD_CNT_9(9, "NOTFOODCNT9", 2),
		NOT_FOOD_CNT_10(10, "NOTFOODCNT10", 2);

		private int count;

		private String name;

		private int type;

		CountEnum(int count, String name, int type) {
			this.count = count;
			this.name = name;
			this.type = type;
		}

		public static String getNameByCount(int count, int type) {
			for (CountEnum countEnum : values()) {
				if (countEnum.getCount() == count && countEnum.getType() == type)
					return countEnum.getName();
			}
			return null;
		}

		public int getCount() {
			return this.count;
		}

		public String getName() {
			return this.name;
		}

		public int getType() {
			return this.type;
		}
	}

	private static double[] splits = new double[] { 1.0E-4D, 0.4D, 2.0D, 4.0D, 8.0D, 13.3333D, 20.0D, 35.0D, 49.3333D };

	private static double[] splits2 = new double[] { 1.0E-4D, 1.0D, 4.0D, 11.0D, 20.0D, 38.0D };

	private static double[] splits3 = new double[] { 1.0E-4D, 3.0D, 8.0D, 15.0D, 22.0D, 40.0D };

	public static String getAddedFeature(String query, double[] splits, Map<String, Integer> foodGramCounter) {
		int nGram = 0;
		double sumFoodGramCnt = 0.0D;
		for (int i = 0; i < query.length() - 1; i++) {
			for (int j = i + 2; j <= Math.min(query.length(), i + 3); j++) {
				String gram = query.substring(i, j);
				nGram++;
				if (foodGramCounter.containsKey(gram))
					sumFoodGramCnt += ((Integer) foodGramCounter.get(gram)).intValue();
			}
		}
		if (nGram == 0)
			return "美食gram频次和除以gram个数_0";
		return "美食gram频次和除以gram个数_" + getBin(splits, Double.valueOf(sumFoodGramCnt / nGram));
	}

	public static String getAddedFeature2(String query, double[] splits, Map<String, Integer> foodGramCounter,
			Set<String> foodWordSet) {
		for (int i = 1; i < query.length() + 1; i++) {
			String prefix = query.substring(0, i);
			if (foodWordSet.contains(prefix) && foodGramCounter.containsKey(prefix))
				return "第一个美食词前缀的频次_"
						+ getBin(splits, Double.valueOf(((Integer) foodGramCounter.get(prefix)).intValue()));
		}
		return "第一个美食词前缀的频次_" + getBin(splits, Double.valueOf(0.0D));
	}

	public static String getAddedFeature3(String query, double[] splits, Map<String, Integer> foodGramCounter,
			Set<String> foodWordSet) {
		for (int i = 1; i <= query.length(); i++) {
			String suffix = query.substring(query.length() - i);
			if (foodWordSet.contains(suffix) && foodGramCounter.containsKey(suffix))
				return "第一个美食词后缀的频次_"
						+ getBin(splits, Double.valueOf(((Integer) foodGramCounter.get(suffix)).intValue()));
		}
		return "第一个美食词后缀的频次_" + getBin(splits, Double.valueOf(0.0D));
	}

	private static String getBin(double[] bin, Double value) {
		for (int i = 0; i < bin.length; i++) {
			if (value.doubleValue() < bin[i])
				return String.valueOf(i);
		}
		return String.valueOf(bin.length);
	}
}