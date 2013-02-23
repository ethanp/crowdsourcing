import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * This didn't end up being necessary
 */
public class Question
{
	int questionID;
	int goldenJudgement;
	double prior = -1;
	Map<String,Integer> workerQs;

	Question(int questionID, int goldenJudgement)
	{
		this.questionID = questionID;
		this.goldenJudgement = goldenJudgement;
		workerQs = new HashMap<String, Integer>();
	}

	public void makePrior()
	{
		int sum = 0;
		int count = 0;
		for (int value : workerQs.values())
		{
			sum += value;
			count += 1;
		}
		this.prior = ((double) sum) / count;
	}

	public Map<String,Integer> getWorkerQs()
	{
		return workerQs;
	}

	public void addWorkerQ(String workerID, int workerJudgement)
	{
		workerQs.put(workerID, workerJudgement);
	}


	public int getQuestionID()
	{
		return questionID;
	}

	public void setQuestionID(int questionID)
	{
		this.questionID = questionID;
	}

	public int getGoldenJudgement()
	{
		return goldenJudgement;
	}

	public void setGoldenJudgement(int goldenJudgement)
	{
		this.goldenJudgement = goldenJudgement;
	}

	public double getPrior()
	{
		return prior;
	}

	public void setPrior(double prior)
	{
		this.prior = prior;
	}
}
