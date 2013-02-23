import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Map;

/**
 * Implementation of the Snow Algorithm
 * TODO: this currently only works on binary data-sets, but it should be extensible
 *
 */
public class Snow
{
	public static void main(String args[]) throws IOException
	{
		// get the file data formatted for use
		ArrayList<String[]> data = getData
			  ("/Users/Ethan/Dropbox/MLease/all_collected_data/rte.standardized.tsv");
		ArrayList<Question> questions = makeQuestions(data);
		for (Question question : questions)
			question.makePrior();
		Map<String, Map> workers = makeWorkers(data);
		makePosteriorProbs(data, questions, workers);
		printItAllOut(questions);
		return;
	}

	public static void printItAllOut(ArrayList<Question> questions)
	{
		for (Question question : questions) {
			System.out.printf("Question ID: %-8d", question.questionID);
			System.out.printf("GoldSays: %-4d", question.goldenJudgement);
			System.out.printf("Prior: %-8.1f", question.prior);
			System.out.printf("ClassifierSays: %-8.3f\n", question.finalScore);
		}

		int correctBefore = 0;
		int correctAfter = 0;
		for (Question question : questions) {
			if (   (question.prior < .5 && question.goldenJudgement == 0)
				  || (question.prior >= .5 && question.goldenJudgement == 1)) {
				correctBefore++;
			}
			if (   (question.finalScore < 0 && question.goldenJudgement == 0)
			    || (question.finalScore >= 0 && question.goldenJudgement == 1)) {
				correctAfter++;
			}
		}
		System.out.printf("\n\nCorrect Before Fancy Maths: %d, or %3.1f%%\n\n" +
			                  "Correct After:              %d, or %3.1f%%\n",
			  correctBefore, 100.0 * correctBefore / questions.size(),
			  correctAfter, 100.0 * correctAfter / questions.size());
	}

	public static void makePosteriorProbs(ArrayList<String[]> data,
	                                                ArrayList<Question> questions,
	                                                Map<String, Map> workers)
	{
		for (Question question : questions)
		{
			double accumulator = Math.log(question.prior / (1 - question.prior));
			for (String workerID : question.workerQs.keySet())
			{
				Map<String, Double> workerWeightMap = workers.get(workerID);
				int workersResponse = question.workerQs.get(workerID);
				double numerator = workerWeightMap.get("probWorkerYesGivenYes");
				double denominator = workerWeightMap.get("probWorkerYesGivenNo");
				if (workersResponse == 1) {
					accumulator += Math.log(numerator / denominator);
				} else {
					accumulator += Math.log((1 - numerator) / (1 - denominator));
				}
			}
			question.finalScore = accumulator;
		}
	}

	public static Map<String, Map> makeWorkers(ArrayList<String[]> data)
	{
		/*
		   Make all the confusion matrices with the following format:
			yesGivYes -> Count(votedYes & goldYes)
			noGivYes  -> Count(votedNo  & goldYes)
			yesGivNo  -> Count(votedYes & goldNo )
			noGivNo   -> Count(votedNo  & goldNo )
 		 */
		// Put all initialized workers in a list
		Map<String, Map> workersMap = new HashMap<String, Map>();
		// Put all the matrices in a list
		int addIndex = 0, index, currentValue;
		for (String[] dataPoint : data)
		{
			String workerIDString = dataPoint[1];
			// workerMatrix doesn't already exist, initialize it,
			// Note: Laplace smoothing is built-in here
			if (!workersMap.containsKey(workerIDString))
			{
				Map<String, Integer> workerMatrix = new HashMap<String, Integer>();
				// if they voted Yes
				if (dataPoint[3].equals("1")) {
					// gold voted Yes
					if (dataPoint[4].equals("1")) {
						workerMatrix.put("yesGivYes", 2);
						workerMatrix.put("noGivYes", 1);
						workerMatrix.put("yesGivNo", 1);
						workerMatrix.put("noGivNo", 1);
					} else { // gold voted No
						workerMatrix.put("yesGivYes", 1);
						workerMatrix.put("noGivYes", 1);
						workerMatrix.put("yesGivNo", 2);
						workerMatrix.put("noGivNo", 1);
					}
				} else {
					// they voted No, gold voted Yes
					if (dataPoint[4].equals("1")) {
						workerMatrix.put("yesGivYes", 1);
						workerMatrix.put("noGivYes", 2);
						workerMatrix.put("yesGivNo", 1);
						workerMatrix.put("noGivNo", 1);
					} else {
						// gold voted No
						workerMatrix.put("yesGivYes", 1);
						workerMatrix.put("noGivYes", 1);
						workerMatrix.put("yesGivNo", 1);
						workerMatrix.put("noGivNo", 2);
					}
				}
				workersMap.put(workerIDString, workerMatrix);
			} else { // worker does already exist, update them
				Map<String, Integer> workerMatrix = workersMap.get(workerIDString);
				if (dataPoint[3].equals("1")) {
					// gold voted Yes
					if (dataPoint[4].equals("1")) {
						currentValue = workerMatrix.remove("yesGivYes") + 1;
						workerMatrix.put("yesGivYes", currentValue);
					} else { // gold voted No
						currentValue = workerMatrix.remove("yesGivNo") + 1;
						workerMatrix.put("yesGivNo", currentValue);
					}
				} else {
					// they voted No, gold voted Yes
					if (dataPoint[4].equals("1")) {
						currentValue = workerMatrix.remove("noGivYes") + 1;
						workerMatrix.put("noGivYes", currentValue);
					} else {
						// gold voted No
						currentValue = workerMatrix.remove("noGivNo") + 1;
						workerMatrix.put("noGivNo", currentValue);
					}
				}
			}
		}
		Map<String, Map> workers = new HashMap<String, Map>();
		for (String workerID : workersMap.keySet())
		{
			// retrieve matrix
			Integer yesGivYes = (Integer) workersMap.get(workerID).get("yesGivYes");
			Integer noGivYes = (Integer) workersMap.get(workerID).get("noGivYes");
			Integer yesGivNo = (Integer) workersMap.get(workerID).get("yesGivNo");
			Integer noGivNo = (Integer) workersMap.get(workerID).get("noGivNo");

			// calculate probabilities
			double probWorkerYesGivenYes = ((double) yesGivYes) / (yesGivYes + noGivYes);
			double probWorkerYesGivenNo = ((double) yesGivNo) / (yesGivNo + noGivNo);

			// store probabilities
			Map<String, Double> workerWeights = new HashMap<String, Double>();
			workerWeights.put("probWorkerYesGivenYes", probWorkerYesGivenYes);
			workerWeights.put("probWorkerYesGivenNo", probWorkerYesGivenNo);
			workers.put(workerID, workerWeights);
		}
		return workers;
	}

	public static ArrayList<Question> makeQuestions(ArrayList<String[]> data)
	{
		ArrayList<Question> questionObjects = new ArrayList<Question>();
		ArrayList<Integer> seenQuestions = new ArrayList<Integer>();
		for (String[] dataPoint : data)
		{
			int workerJudgement = Integer.parseInt(dataPoint[3]);
			int questionID = Integer.parseInt(dataPoint[2]);
			String workerID = dataPoint[1];

			// If this Question hasn't been seen, initialize a Question for it
			if (!seenQuestions.contains(questionID))
			{
				int goldenJudgement = Integer.parseInt(dataPoint[4]);

				// Make Question object and add it to the list
				Question question = new Question(questionID, goldenJudgement);
				question.addWorkerQ(workerID, workerJudgement);
				questionObjects.add(question);

				seenQuestions.add(questionID);

			} else {
			// Otherwise add worker & judgement to the appropriate Question object
				for (Question question : questionObjects) {
					if (question.questionID == questionID) {
						question.addWorkerQ(workerID, workerJudgement);
						break;
					}
				}
			}
		}

		return questionObjects;
	}

	public static ArrayList<String[]> getData(String file) throws IOException
	{
		// Read in the file 'fileName'
		ArrayList<String[]> fileList = new ArrayList<String[]>();
		BufferedReader br = new BufferedReader(new FileReader(file));
		String line;
		while ((line = br.readLine()) != null) {
			// break the line into its constituent pieces
			String[] columns = new String[5];
			columns = line.split("\t");
			fileList.add(columns);
		}
		br.close();
		fileList.remove(0);
		return fileList;
	}
}
