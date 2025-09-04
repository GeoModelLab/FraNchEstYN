// This class defines a simulation unit: site, crop, variety, pathogen, geographical location, resistance level, and sowing/reference data.

public class simulationUnit
{
    public string site { get; set; }                                 // site identifier (e.g., location name)
    public string crop { get; set; }                                 // crop type (e.g., wheat, maize)
    public float latitude { get; set; }                              // degrees - site latitude
    public float longitude { get; set; }                             // degrees - site longitude
    public string variety { get; set; }                              // variety name of the crop
    public string pathogen { get; set; }                             // pathogen name (e.g., rust, mildew)
    public float varietyResistance { get; set; }                     // 0–1 - varietal resistance to pathogen

    public Dictionary<int, int> year_sowingDOY = new Dictionary<int, int>(); // [year] = day of year - maps each simulation year to its sowing day

    public referenceData referenceData = new referenceData();        // observed or reference data for validation

    public fungicideTreatmentSchedule fungicideTreatmentSchedule = new fungicideTreatmentSchedule();  // fungicide treatment scheduling
}

//fungicide treatment scheduling
public class fungicideTreatmentSchedule
{
    public List<DateTime> Treatments { get; set; } = new List<DateTime>();     // all dates when a fungicide treatment is scheduled

    public void AddTreatment(DateTime treatmentDate)
    {
        Treatments.Add(treatmentDate); // method to add a treatment date
    }
}

// This class stores observed or reference data for a simulation unit, including biomass, yield, and disease severity over time.
public class referenceData
{
    public Dictionary<DateTime, float> date_fint = new Dictionary<DateTime, float>();             // [date] = fraction of light interception (fraction)

    public Dictionary<DateTime, float> date_agb = new Dictionary<DateTime, float>();              // [date] = above-ground biomass (kg ha-1)

    public Dictionary<DateTime, float> date_yieldActual = new Dictionary<DateTime, float>();      // [date] = actual yield (kg ha-1)

    public Dictionary<DateTime, float> date_yieldAttainable = new Dictionary<DateTime, float>();  // [date] = attainable yield (kg ha-1)

    public Dictionary<string, Dictionary<DateTime, float>> disease_date_diseaseSev =
        new Dictionary<string, Dictionary<DateTime, float>>();                                    // [pathogen][date] = disease severity (% or fraction) for a given pathogen
}

