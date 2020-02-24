using System.Collections;
using System.Collections.Generic;
using UnityEngine;
using System.IO;

public class DataRecorder : MonoBehaviour {

	private ModelBehavior model;
	private string FileName;
	private StreamWriter writer;


	public void Initialize () 
	{

		// Connect to model
        model = GetComponent<ModelBehavior>();

		// Open new file for writing and give it column names
		GetFileName ();
		writer = new StreamWriter(FileName, false);
        writer.WriteLine("Step, Time, PreyPop, PredPop, MutantPop, PreyInCaptureRange, PreyInCaptureRangeQuesting, PreyInCaptureRangeHandling, PreyConsumed, PredHandling, Polarization, Crowding, CrowdingQuesting, CrowdingHandling, CollectivePred, CollectivePrey, NumberOfGroups, MeanGroupSize, VarGroupSize, MaxGroupSize, NumberOfSingletonGroups, NumberOfMutantGroups, PreyInCaptureRangeMutant, LocalConspecificsMutant, LocalHeterospecificsMutant");

	}


	public void SaveNow ()
	{

		writer.WriteLine (
            model.SimulationStep                        + "," +
            model.SimulationTime 		                + "," + 
			model.PreyPopulationSize 	                + "," +
			model.PredPopulationSize	                + "," +
            model.MutantPopulationSize                  + "," +
            model.PreyInCaptureRange                    + "," +
            model.PreyInCaptureRangeQuesting            + "," +
            model.PreyInCaptureRangeHandling            + "," +
            model.PreyConsumed                          + "," +
            model.PredHandling                          + "," +
            model.Polarization                          + "," +
            model.Crowding                              + "," +
            model.CrowdingQuesting                      + "," +
            model.CrowdingHandling                      + "," +
            model.CollectivePred                        + "," +
            model.CollectivePrey                        + "," +
            model.NumberOfGroups                        + "," +
            model.MeanGroupSize                         + "," +
            model.VarGroupSize                          + "," +
            model.MaxGroupSize                          + "," +
            model.NumberOfSingletonGroups               + "," +
            model.NumberOfMutantGroups                  + "," +
            model.PreyInCaptureRangeMutant              + "," +
            model.LocalConspecificsMutant               + "," +
            model.LocalHeterospecificsMutant    
            );

	}




	public void CloseNow ()
	{
		writer.Close ();
	}



	void GetFileName () 
	{

		FileName = model.OutputPath;
        FileName += "d";
		FileName += model.RunID;
		FileName += ".csv";

	}


}
