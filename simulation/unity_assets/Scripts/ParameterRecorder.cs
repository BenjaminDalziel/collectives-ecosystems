using System.Collections;
using System.Collections.Generic;
using UnityEngine;
using System.IO;

public class ParameterRecorder : MonoBehaviour {

    private ModelBehavior model;
    private string FileName;
    private StreamWriter writer;


    public void Initialize()
    {

        // Connect to model
        model = GetComponent<ModelBehavior>();

      
        // Open new file for writing and give it column names 
        // that match exactly the parameters
        // as shown in SaveNow method below
        GetFileName();
        writer = new StreamWriter(FileName, false);
        writer.WriteLine("RunID, " +
                         "MaxCommunitySize, " +
                         "InitialPreyPopulationSize, " +
                         "InitialPredPopulationSize, " +
                         "InitialMutantPopulationSize, " +
                         "TimeStep, " +
                         "BirthRate, " +
                         "CarryingCapacity, " +
                         "InteractionRadius, " +
                         "RepulsionRadius, " +
						 "AttractionRadius, " +
			             "AlignmentRadius, " +
                         "CaptureRadius, " +
                         "MaxTurnRate, " +
                         "CaptureRate, " +
                         "MutantAdvantage," +
                         "ConversionProbability, " +
                         "DeathRate," +
                         "HandlingTime," +
                         "MetabolicRate," +
                         "PreyNoise," +
                         "PredNoise," +
                         "ArenaLength," +
                         "ArenaLengthInBuckets," +
                         "PreySpeed," +
                         "PredSpeed," +
                         "Pursuit," +
                         "Avoidance," +
                         "DeviantMutant"
                        );

    }


    public void SaveNow()
    {

        writer.WriteLine(
            model.RunID + "," +
            model.MaxCommunitySize + "," +
            model.InitialPreyPopulationSize + "," +
            model.InitialPredPopulationSize + "," +
            model.InitialMutantPopulationSize + "," +
            model.TimeStep + "," +
            model.BirthRate + "," +
            model.CarryingCapacity + "," +
            model.InteractionRadius + "," +
            model.RepulsionRadius + "," +
			model.AttractionRadius + "," +
			model.AlignmentRadius + "," +
            model.CaptureRadius + "," +
            model.MaxTurnRate + "," +
            model.CaptureRate + "," +
            model.MutantAdvantage + "," +
            model.ConversionProbability + "," +
            model.DeathRate + "," +
            model.HandlingTime + "," +
            model.MetabolicRate + "," +
            model.PreyNoise + "," +
            model.PredNoise + "," +
            model.ArenaLength + "," +
            model.ArenaLengthInBuckets + "," +
            model.PreySpeed + "," +
            model.PredSpeed + "," +
            model.Pursuit + "," +
            model.Avoidance + "," +
            model.DeviantMutant
        );

    }




    public void CloseNow()
    {
        writer.Close();
    }



    void GetFileName()
    {

        FileName = model.OutputPath;
        FileName += "p";
        FileName += model.RunID;
        FileName += ".csv";

    }
}
